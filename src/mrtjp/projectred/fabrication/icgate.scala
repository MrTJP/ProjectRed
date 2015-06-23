/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.vec.Transformation
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.util.Enum
import net.minecraft.nbt.NBTTagCompound

abstract class GateICPart extends CircuitPart with TConnectableICPart with TICOrient with IGuiCircuitPart
{
    private var gateSubID:Byte = 0
    private var gateShape:Byte = 0

    var schedTime = 0L
    var schedDigital = false

    def getLogic[T]:T
    def getLogicPrimitive = getLogic[ICGateLogic[GateICPart]]

    def subID = gateSubID&0xFF

    def shape = gateShape&0xFF
    def setShape(s:Int){ gateShape = s.toByte }

    def preparePlacement(rot:Int, meta:Int)
    {
        gateSubID = meta.toByte
        setRotation(rot)
    }

    override def save(tag:NBTTagCompound)
    {
        tag.setByte("orient", orientation)
        tag.setByte("subID", gateSubID)
        tag.setByte("shape", gateShape)
        tag.setByte("connMap", connMap)
        tag.setLong("schedTime", schedTime)
    }

    override def load(tag:NBTTagCompound)
    {
        orientation = tag.getByte("orient")
        gateSubID = tag.getByte("subID")
        gateShape = tag.getByte("shape")
        connMap = tag.getByte("connMap")
        schedTime = tag.getLong("schedTime")
    }

    override def writeDesc(out:MCDataOutput)
    {
        out.writeByte(orientation)
        out.writeByte(gateSubID)
        out.writeByte(gateShape)
    }

    override def readDesc(in:MCDataInput)
    {
        orientation = in.readByte()
        gateSubID = in.readByte()
        gateShape = in.readByte()
    }

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 1 => orientation = in.readByte()
        case 2 => gateShape = in.readByte()
        case _ => super.read(in, key)
    }

    override def readClientPacket(in:MCDataInput)
    {
        readClientPacket(in, in.readUByte())
    }

    def readClientPacket(in:MCDataInput, key:Int) = key match
    {
        case 0 => rotate()
        case 1 => configure()
        case 2 => getLogicPrimitive.activate(this)
        case _ =>
    }

    override def canConnectPart(part:CircuitPart, r:Int) =
        getLogicPrimitive.canConnectTo(this, part, toInternal(r))

    override def scheduledTick()
    {
        getLogicPrimitive.scheduledTick(this)
    }

    override def scheduleTick(ticks:Int)
    {
        if (ticks == 0) scheduleDigitalTick()
        else if (schedTime < 0) schedTime = world.network.getWorld.getTotalWorldTime+ticks
    }

    def processScheduled()
    {
        if (schedTime >= 0 && world.network.getWorld.getTotalWorldTime >= schedTime)
        {
            schedTime = -1
            scheduledTick()
        }
    }

    def scheduleDigitalTick()
    {
        schedDigital = true
    }

    var iter = 0
    def processScheduledDigital()
    {
        while(schedDigital && iter < 3) //recursion control
        {
            schedDigital = false
            iter += 1
            scheduledTick()
        }
    }

    def onChange()
    {
        processScheduled()
        getLogicPrimitive.onChange(this)
        processScheduledDigital()
    }

    override def update()
    {
        if (!world.network.isRemote)
        {
            processScheduled()
            iter = 0
            processScheduledDigital()
        }
        getLogicPrimitive.onTick(this)
    }

    override def onNeighborChanged()
    {
        if (!world.network.isRemote)
        {
            updateConns()
            onChange()
        }
    }

    override def onAdded()
    {
        super.onAdded()
        if (!world.network.isRemote)
        {
            getLogicPrimitive.setup(this)
            updateConns()
            onChange()
        }
    }

    override def onRemoved()
    {
        super.onRemoved()
        if (!world.network.isRemote) notify(0xF)
    }

    def configure()
    {
        if (getLogicPrimitive.cycleShape(this))
        {
            updateConns()
            world.network.markDirty()
            sendShapeUpdate()
            notify(0xF)
            onChange()
        }
    }

    def rotate()
    {
        setRotation((rotation+1)%4)
        updateConns()
        world.network.markDirty()
        sendOrientUpdate()
        notify(0xF)
        onChange()
    }

    def sendShapeUpdate()
    {
        writeStreamOf(2).writeByte(gateShape)
    }

    def sendOrientUpdate()
    {
        writeStreamOf(1).writeByte(orientation)
    }

    @SideOnly(Side.CLIENT)
    override def renderDynamic(t:Transformation, ortho:Boolean, frame:Float)
    {
        RenderICGate.renderDynamic(this, t, ortho, frame)
    }

    @SideOnly(Side.CLIENT)
    override def getPartName = ICGateDefinition(subID).name

    @SideOnly(Side.CLIENT)
    override def getRolloverData(detailLevel:Int) =
    {
        import net.minecraft.util.EnumChatFormatting._
        val s = Seq.newBuilder[String]
        if (detailLevel >= 2) s += "rotation: "+rotation
        s ++= getLogicPrimitive.getRolloverData(this, detailLevel)
        super.getRolloverData(detailLevel)++s.result().map(GRAY+_)
    }

    @SideOnly(Side.CLIENT)
    override def createGui = getLogicPrimitive.createGui(this)

    @SideOnly(Side.CLIENT)
    override def onClicked()
    {
        sendClientPacket(_.writeByte(2))
    }

    @SideOnly(Side.CLIENT)
    override def getPickOp =
        CircuitOpDefs.values(CircuitOpDefs.SimpleIO.ordinal+subID).getOp
}

abstract class ICGateLogic[T <: GateICPart]
{
    def canConnectTo(gate:T, part:CircuitPart, r:Int):Boolean

    def cycleShape(gate:T) = false

    def onChange(gate:T)

    def scheduledTick(gate:T)

    def onTick(gate:T){}

    def setup(gate:T){}

    def activate(gate:T){}

    def getRolloverData(gate:T, detailLevel:Int):Seq[String] = Seq.empty

    def createGui(gate:T):CircuitGui = new ICGateGui(gate)
}

trait TComplexGateICPart extends GateICPart
{
    def getLogicComplex = getLogic[TComplexICGateLogic[TComplexGateICPart]]

    def assertLogic()

    abstract override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        getLogicComplex.save(tag)
    }

    abstract override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        assertLogic()
        getLogicComplex.load(tag)
    }

    abstract override def writeDesc(packet:MCDataOutput)
    {
        super.writeDesc(packet)
        getLogicComplex.writeDesc(packet)
    }

    abstract override def readDesc(packet:MCDataInput)
    {
        super.readDesc(packet)
        assertLogic()
        getLogicComplex.readDesc(packet)
    }

    abstract override def read(packet:MCDataInput, key:Int) = key match
    {
        case k if k > 10 => getLogicComplex.read(packet, k)
        case _ => super.read(packet, key)
    }

    abstract override def preparePlacement(rot:Int, meta:Int)
    {
        super.preparePlacement(rot, meta)
        assertLogic()
    }
}

trait TComplexICGateLogic[T <: TComplexGateICPart] extends ICGateLogic[T]
{
    def save(tag:NBTTagCompound){}
    def load(tag:NBTTagCompound){}

    def readDesc(packet:MCDataInput){}
    def writeDesc(packet:MCDataOutput){}

    def read(packet:MCDataInput, key:Int){}
}

object ICGateDefinition extends Enum
{
    type EnumVal = ICGateDef

    val IOSimple = ICGateDef("Simple IO", CircuitPartDefs.IOGate.id)
    val IOAnalog = ICGateDef("Analog IO", CircuitPartDefs.IOGate.id)
    val IOBundled = ICGateDef("Bundled IO", CircuitPartDefs.IOGate.id)

    val OR = ICGateDef("OR gate", CircuitPartDefs.SimpleGate.id)
    val NOR = ICGateDef("NOR gate", CircuitPartDefs.SimpleGate.id)
    val NOT = ICGateDef("NOT gate", CircuitPartDefs.SimpleGate.id)
    val AND = ICGateDef("AND gate", CircuitPartDefs.SimpleGate.id)
    val NAND = ICGateDef("NAND gate", CircuitPartDefs.SimpleGate.id)
    val XOR = ICGateDef("XOR gate", CircuitPartDefs.SimpleGate.id)
    val XNOR = ICGateDef("XNOR gate", CircuitPartDefs.SimpleGate.id)
    val Buffer = ICGateDef("Buffer gate", CircuitPartDefs.SimpleGate.id)
    val Multiplexer = ICGateDef("Multiplexer", CircuitPartDefs.SimpleGate.id)
    val Pulse = ICGateDef("Pulse Former", CircuitPartDefs.SimpleGate.id)
    val Repeater = ICGateDef("Repeater", CircuitPartDefs.SimpleGate.id)

    case class ICGateDef(unlocal:String, gateType:Int) extends Value
    {
        override def name = unlocal
    }
}