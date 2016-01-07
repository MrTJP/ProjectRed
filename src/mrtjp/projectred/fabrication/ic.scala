/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.packet.PacketCustom
import codechicken.lib.vec.{BlockCoord, Transformation}
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.util.Enum
import mrtjp.core.vec.{Point, Size}
import mrtjp.projectred.ProjectRedCore.log
import net.minecraft.item.ItemStack
import net.minecraft.nbt.{NBTTagCompound, NBTTagList}
import net.minecraft.world.World
import net.minecraftforge.fluids.FluidStack

import scala.collection.mutable.{Map => MMap, Seq => MSeq}

trait WorldCircuit
{
    def getIC:IntegratedCircuit
    def getWorld:World

    def getICStreamOf(key:Int):MCDataOutput
    def getPartStream(x:Int, y:Int):MCDataOutput

    def isRemote:Boolean
    def markSave()
}

object DummyMCIO extends MCDataOutput
{
    override def writeVarInt(i:Int) = this
    override def writeCoord(x:Int, y:Int, z:Int) = this
    override def writeCoord(coord:BlockCoord) = this
    override def writeString(s:String) = this
    override def writeFloat(f:Float) = this
    override def writeDouble(d:Double) = this
    override def writeShort(s:Int) = this
    override def writeVarShort(s:Int) = this
    override def writeInt(i:Int) = this
    override def writeFluidStack(liquid:FluidStack) = this
    override def writeByteArray(array:Array[Byte]) = this
    override def writeBoolean(b:Boolean) = this
    override def writeItemStack(stack:ItemStack) = this
    override def writeNBTTagCompound(tag:NBTTagCompound) = this
    override def writeChar(c:Char) = this
    override def writeLong(l:Long) = this
    override def writeByte(b:Int) = this
}

trait SimulatedWorldCircuit extends WorldCircuit
{
    override def getICStreamOf(key:Int) = DummyMCIO
    override def getPartStream(x:Int, y:Int) = DummyMCIO
    override def isRemote = false
}

trait NetWorldCircuit extends WorldCircuit
{
    private var icStream:PacketCustom = null
    private var partStream:PacketCustom = null

    def createPartStream():PacketCustom
    def sendPartStream(out:PacketCustom)
    override def getPartStream(x:Int, y:Int):MCDataOutput =
    {
        if (partStream == null) partStream = createPartStream()

        val part = getIC.getPart(x, y)
        partStream.writeByte(part.id)
        partStream.writeByte(x).writeByte(y)

        partStream
    }
    def flushPartStream()
    {
        if (partStream != null)
        {
            partStream.writeByte(255)//terminator
            sendPartStream(partStream.compress())
            partStream = null
        }
    }
    def readPartStream(in:MCDataInput)
    {
        try
        {
            var id = in.readUByte()
            while (id != 255)
            {
                val (x, y) = (in.readUByte(), in.readUByte())
                var part = getIC.getPart(x, y)
                if (part == null || part.id != id)
                {
                    log.error("client part stream couldnt find part "+Point(x, y))
                    part = CircuitPart.createPart(id)
                }
                part.read(in)
                id = in.readUByte()
            }
        }
        catch
            {
                case ex:IndexOutOfBoundsException =>
                    log.error("Circuit part stream failed to be read.")
                    ex.printStackTrace()
            }
    }

    def createICStream():PacketCustom
    def sendICStream(out:PacketCustom)
    override def getICStreamOf(key:Int):MCDataOutput =
    {
        if (icStream == null) icStream = createICStream()
        icStream.writeByte(key)
        icStream
    }
    def flushICStream()
    {
        if (icStream != null)
        {
            icStream.writeByte(255) //terminator
            sendICStream(icStream.compress())
            icStream = null
        }
    }
    def readICStream(in:MCDataInput)
    {
        try
        {
            var id = in.readUByte()
            while (id != 255)
            {
                getIC.read(in, id)
                id = in.readUByte()
            }
        }
        catch
            {
                case ex:IndexOutOfBoundsException =>
                    log.error("Circuit IC stream failed to be read")
            }
    }
}

class IntegratedCircuit
{
    var network:WorldCircuit = null

    var name = "untitled"
    var size = Size.zeroSize

    var parts = MMap[(Int, Int), CircuitPart]()
    var errors = Map.empty[Point, (String, Int)]

    private var scheduledTicks = MMap[(Int, Int), Long]()

    /**
     * Mapped inputs and outputs of this IC.
     * Outputs go to the world, inputs come in from the world.
     * OOOO OOOO OOOO OOOO IIII IIII IIII IIII
     */
    val iostate = Array(0, 0, 0, 0)

    var outputChangedDelegate = {() => ()}

    def setInput(r:Int, state:Int)
    {
        iostate(r) = iostate(r)&0xFFFF0000|state&0xFFFF
    }

    def setOutput(r:Int, state:Int)
    {
        iostate(r) = iostate(r)&0xFFFF|(state&0xFFFF)<<16
    }

    def onInputChanged(mask:Int)
    {
        val ioparts = parts.values.collect{case io:IIOCircuitPart => io}
        for (r <- 0 until 4) if ((mask&1<<r) != 0)
        {
            ioparts.foreach(_.onExtInputChanged(r))
            sendInputUpdate(r)
        }
    }

    def onOutputChanged(mask:Int)
    {
        val ioparts = parts.values.collect{case io:IIOCircuitPart => io}
        for (r <- 0 until 4) if ((mask&1<<r) != 0)
        {
            ioparts.foreach(_.onExtOutputChanged(r))
            outputChangedDelegate()
            sendOutputUpdate(r)
        }
    }

    def save(tag:NBTTagCompound)
    {
        tag.setString("name", name)
        tag.setByte("sw", size.width.toByte)
        tag.setByte("sh", size.height.toByte)
        tag.setIntArray("iost", iostate)

        val tagList = new NBTTagList
        for (part <- parts.values)
        {
            val partTag = new NBTTagCompound
            partTag.setByte("id", part.id.toByte)
            partTag.setByte("xpos", part.x.toByte)
            partTag.setByte("ypos", part.y.toByte)
            part.save(partTag)
            tagList.appendTag(partTag)
        }
        tag.setTag("parts", tagList)

        //etc
    }

    def load(tag:NBTTagCompound)
    {
        clear()
        name = tag.getString("name")
        size = Size(tag.getByte("sw")&0xFF, tag.getByte("sh")&0xFF)
        val ta = tag.getIntArray("iost")
        for (i <- 0 until 4) iostate(i) = ta(i)

        val partList = tag.getTagList("parts", 10)
        for(i <- 0 until partList.tagCount)
        {
            val partTag = partList.getCompoundTagAt(i)
            val part = CircuitPart.createPart(partTag.getByte("id")&0xFF)
            setPart_do(partTag.getByte("xpos")&0xFF, partTag.getByte("ypos")&0xFF, part)
            part.load(partTag)
        }

        //etc
    }

    def writeDesc(out:MCDataOutput)
    {
        out.writeString(name)
        out.writeByte(size.width).writeByte(size.height)
        for (i <- 0 until 4) out.writeInt(iostate(i))

        for (((x, y), part) <- parts)
        {
            out.writeByte(part.id)
            out.writeByte(x).writeByte(y)
            part.writeDesc(out)
        }
        out.writeByte(255)

        //etc
    }

    def readDesc(in:MCDataInput)
    {
        clear()
        name = in.readString()
        size = Size(in.readUByte(), in.readUByte())
        for (i <- 0 until 4) iostate(i) = in.readInt()

        var id = in.readUByte()
        while (id != 255)
        {
            val part = CircuitPart.createPart(id)
            setPart_do(in.readUByte(), in.readUByte(), part)
            part.readDesc(in)
            id = in.readUByte()
        }
        //etc
    }

    def read(in:MCDataInput, key:Int) = key match
    {
        case 0 => readDesc(in)
        case 1 =>
            val part = CircuitPart.createPart(in.readUByte())
            setPart_do(in.readUByte(), in.readUByte(), part)
            part.readDesc(in)
        case 2 => removePart(in.readUByte(), in.readUByte())
        case 3 => CircuitOp.getOperation(in.readUByte()).readOp(this, in)
        case 4 => getPart(in.readUByte(), in.readUByte()) match
        {
            case g:TClientNetCircuitPart => g.readClientPacket(in)
            case _ => log.error("Server IC stream received invalid client packet")
        }
        case 5 => iostate(in.readUByte()) = in.readInt()
        case 6 => setInput(in.readUByte(), in.readShort())
        case 7 => setOutput(in.readUByte(), in.readShort())
        case _ =>
    }

    def sendPartAdded(part:CircuitPart)
    {
        val out = network.getICStreamOf(1)
        out.writeByte(part.id)
        out.writeByte(part.x).writeByte(part.y)
        part.writeDesc(out)
    }

    def sendRemovePart(x:Int, y:Int)
    {
        network.getICStreamOf(2).writeByte(x).writeByte(y)
    }

    def sendOpUse(op:CircuitOp, start:Point, end:Point) =
    {
        if (op.checkOp(this, start, end))
        {
            op.writeOp(this, start, end, network.getICStreamOf(3).writeByte(op.id))
            true
        }
        else false
    }

    def sendClientPacket(part:TClientNetCircuitPart, writer:MCDataOutput => Unit)
    {
        val s = network.getICStreamOf(4).writeByte(part.x).writeByte(part.y)
        writer(s)
    }

    def sendIOUpdate(r:Int)
    {
        network.getICStreamOf(5).writeByte(r).writeInt(iostate(r))
    }

    def sendInputUpdate(r:Int)
    {
        network.getICStreamOf(6).writeByte(r).writeShort(iostate(r)&0xFFFF)
    }

    def sendOutputUpdate(r:Int)
    {
        network.getICStreamOf(7).writeByte(r).writeShort(iostate(r)>>16)
    }

    def clear()
    {
        parts.values.foreach{_.unbind()}//remove references
        parts = MMap()
        scheduledTicks = MMap()
        name = "untitled"
        size = Size.zeroSize
        for (i <- 0 until 4) iostate(i) = 0
    }

    def isEmpty = size == Size.zeroSize
    def nonEmpty = !isEmpty

    private def assertCoords(x:Int, y:Int)
    {
        if (!(0 until size.width contains x) || !(0 until size.height contains y))
            throw new IndexOutOfBoundsException("Circuit does not contain "+Point(x, y))
    }

    def tick()
    {
        val t = network.getWorld.getTotalWorldTime
        var rem = Seq[(Int, Int)]()
        for((k, v) <- scheduledTicks) if(v >= t)
        {
            getPart(k._1, k._2).scheduledTick()
            rem :+= k
        }
        rem.foreach(scheduledTicks.remove)

        for(part <- parts.values) part.update()
    }

    def refreshErrors()
    {
        val eparts = parts.values.collect {case p:IErrorCircuitPart => p}
        val elist = Map.newBuilder[Point, (String, Int)]

        for (part <- eparts)
        {
            val error = part.postErrors
            if (error != null)
                elist += Point(part.x, part.y) -> error
        }

        errors = elist.result()
    }

    def setPart(x:Int, y:Int, part:CircuitPart)
    {
        setPart_do(x, y, part)
        part.onAdded()
        if (!network.isRemote) sendPartAdded(part)
    }
    private def setPart_do(x:Int, y:Int, part:CircuitPart)
    {
        assertCoords(x, y)
        part.bind(this, x, y)
        parts += (x, y) -> part
    }

    def getPart(x:Int, y:Int):CircuitPart = parts.getOrElse((x, y), null)

    def removePart(x:Int, y:Int)
    {
        assertCoords(x, y)
        val part = getPart(x, y)
        if (part != null)
        {
            if (!network.isRemote) sendRemovePart(x, y)
            parts.remove((x, y))
            part.onRemoved()
            part.unbind()
        }
    }

    def notifyNeighbor(x:Int, y:Int)
    {
        val part = getPart(x, y)
        if (part != null) part.onNeighborChanged()
    }

    def notifyNeighbors(x:Int, y:Int, mask:Int)
    {
        for(r <- 0 until 4) if ((mask&1<<r) != 0)
        {
            val point = Point(x, y).offset(r)
            val part = getPart(point.x, point.y)
            if (part != null) part.onNeighborChanged()
        }
    }

    def scheduleTick(x:Int, y:Int, ticks:Int){scheduledTicks += (x, y) -> (network.getWorld.getTotalWorldTime+ticks)}

    //Convinience functions
    def setPart(p:Point, part:CircuitPart){setPart(p.x, p.y, part)}
    def getPart(p:Point):CircuitPart = getPart(p.x, p.y)
    def removePart(p:Point){removePart(p.x, p.y)}
    def notifyNeighbor(p:Point){notifyNeighbor(p.x, p.y)}
    def notifyNeighbors(p:Point, mask:Int){notifyNeighbors(p.x, p.y, mask)}
    def scheduleTick(p:Point, ticks:Int){scheduleTick(p.x, p.y, ticks)}
}

object CircuitPartDefs extends Enum
{
    type EnumVal = CircuitPartDef

    val Torch = CircuitPartDef(() => new TorchICPart)
    val Lever = CircuitPartDef(() => new LeverICPart)
    val Button = CircuitPartDef(() => new ButtonICPart)

    val AlloyWire = CircuitPartDef(() => new AlloyWireICPart)
    val InsulatedWire = CircuitPartDef(() => new InsulatedWireICPart)
    val BundledCable = CircuitPartDef(() => new BundledCableICPart)

    val IOGate = CircuitPartDef(() => new IOGateICPart)
    val SimpleGate = CircuitPartDef(() => new ComboGateICPart)
    val ComplexGate = CircuitPartDef(() => new SequentialGateICPart)
    val ArrayGate = CircuitPartDef(() => new ArrayGateICPart)

    case class CircuitPartDef(factory:() => CircuitPart) extends Value
    {
        def id = ordinal
        override def name = s"$id"

        def createPart = factory.apply()
    }
}

object CircuitPart
{
    def createPart(id:Int) = CircuitPartDefs(id).createPart
}

abstract class CircuitPart
{
    var world:IntegratedCircuit = null
    var loc:(Byte, Byte) = null

    def bind(ic:IntegratedCircuit, x:Int, y:Int)
    {
        world = ic
        loc = (x.toByte, y.toByte)
    }

    def unbind()
    {
        world = null
        loc = null
    }

    def x = loc._1&0xFF
    def y = loc._2&0xFF
    def id = getPartType.id

    def getPartType:CircuitPartDefs.CircuitPartDef

    def save(tag:NBTTagCompound){}
    def load(tag:NBTTagCompound){}

    def writeDesc(out:MCDataOutput){}
    def readDesc(in:MCDataInput){}

    def writeStreamOf(key:Int):MCDataOutput = world.network.getPartStream(x, y).writeByte(key)
    def read(in:MCDataInput) { read(in, in.readUByte()) }
    def read(in:MCDataInput, key:Int) = key match
    {
        case 0 => readDesc(in)
        case _ =>
    }

    def sendDescUpdate() { writeDesc(writeStreamOf(0)) }

    def update(){}
    def scheduledTick(){}
    def scheduleTick(ticks:Int){ world.scheduleTick(x, y, ticks) }

    def onAdded(){}
    def onRemoved(){}

    def onNeighborChanged(){}

    @SideOnly(Side.CLIENT)
    def onClicked(){}
    @SideOnly(Side.CLIENT)
    def onActivated(){}

    @SideOnly(Side.CLIENT)
    def getPartName:String
    @SideOnly(Side.CLIENT)
    def getPickOp:CircuitOp = null
    @SideOnly(Side.CLIENT)
    def getRolloverData(detailLevel:Int):Seq[String] =
        if (detailLevel > 0) Seq(getPartName) else Seq.empty

    @SideOnly(Side.CLIENT)
    def renderDynamic(t:Transformation, ortho:Boolean, frame:Float){}
}

trait TClientNetCircuitPart extends CircuitPart
{
    def readClientPacket(in:MCDataInput)

    @SideOnly(Side.CLIENT)
    def sendClientPacket(writer:MCDataOutput => Unit = {_ => })
    {
        world.sendClientPacket(this, writer)
    }
}

trait IErrorCircuitPart extends CircuitPart
{
    def postErrors:(String, Int)//(message, colour)
}

trait IGuiCircuitPart extends TClientNetCircuitPart
{
    @SideOnly(Side.CLIENT)
    def createGui:CircuitGui
}

trait IPoweredCircuitPart
{
    def rsOutputLevel(r:Int):Int
    def canConnectRS(r:Int):Boolean
}