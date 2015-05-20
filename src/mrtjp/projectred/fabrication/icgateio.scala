/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.color.Colors
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.EnumChatFormatting

trait IIOCircuitPart
{
    def onExtInputChanged(r:Int)
    def onExtOutputChanged(r:Int)

    def getIOSide:Int
    def getIOMode:Int
    def getConnMode:Int
}

object IIOCircuitPart
{
    val Closed = 0
    val Input = 1
    val Output = 2
    val InOut = 3

    val NoConn = 0
    val Simple = 1
    val Analog = 2
    val Bundled = 3
}

class IOGateICPart extends RedstoneGateICPart with IIOCircuitPart with TComplexGateICPart
{
    private var logic:IOICGateLogic = null

    override def getLogic[T] = logic.asInstanceOf[T]
    def getLogicIO = getLogic[IOICGateLogic]

    override def assertLogic()
    {
        if (logic == null) logic = IOICGateLogic.create(this, subID)
    }

    override def readClientPacket(in:MCDataInput, key:Int) = key match
    {
        case 5 => getLogicIO match
        {
            case f:TFreqIOICGateLogic => f.freqUp()
            case _ =>
        }
        case 6 => getLogicIO match
        {
            case f:TFreqIOICGateLogic => f.freqDown()
            case _ =>
        }
        case _ => super.readClientPacket(in, key)
    }

    override def getPartType = CircuitPartDefs.IOGate

    override def onExtInputChanged(r:Int)
    {
        if (r == rotation) getLogicIO.extInputChange(this)
    }
    override def onExtOutputChanged(r:Int)
    {
        if (r == rotation) getLogicIO.extOutputChange(this)
    }
    override def getIOSide = rotation
    override def getIOMode = getLogicIO.getIOMode(this)
    override def getConnMode = getLogicIO.getConnMode(this)

    override def getRedstoneInput(r:Int):Int =
    {
        if (r == 0) getLogicIO.resolveInputFromWorld //r is to outside world
        else super.getRedstoneInput(r)
    }

    override def onOutputChange(mask:Int)
    {
        super.onOutputChange(mask)
        if ((mask&1) != 0)
        {
            val oldOutput = world.iostate(rotation)>>>16
            getLogicIO.setWorldOutput((state&0x10) != 0)
            val newOutput = world.iostate(rotation)>>>16
            if (oldOutput != newOutput) world.onOutputChanged(1<<rotation)
        }
    }

    @SideOnly(Side.CLIENT)
    override def createGui:CircuitGui = getLogicIO.createGui(this)
}

object IOICGateLogic
{
    import mrtjp.projectred.fabrication.{ICGateDefinition => defs}
    def create(gate:IOGateICPart, subID:Int) = subID match
    {
        case defs.IOSimple.ordinal => new SimpleIOICGateLogic(gate)
        case defs.IOAnalog.ordinal => new AnalogIOICGateLogic(gate)
        case defs.IOBundled.ordinal => new BundledIOICGateLogic(gate)
        case _ => null
    }
}

abstract class IOICGateLogic(val gate:IOGateICPart) extends RedstoneICGateLogic[IOGateICPart] with TComplexICGateLogic[IOGateICPart]
{
    import IIOCircuitPart._

    override def inputMask(shape:Int) = shape match
    {
        case 0 => 1
        case 1 => 4
        case 2 => 5
    }
    override def outputMask(shape:Int) = shape match
    {
        case 0 => 4
        case 1 => 1
        case 2 => 5
    }

    override def cycleShape(gate:IOGateICPart) =
    {
        gate.setShape((gate.shape+1)%3)
        true
    }

    def extInputChange(gate:IOGateICPart){onChange(gate)}
    def extOutputChange(gate:IOGateICPart){}
    def getIOMode(gate:IOGateICPart):Int = gate.shape match
    {
        case 0 => Input
        case 1 => Output
        case 2 => InOut
    }
    def getConnMode(gate:IOGateICPart):Int

    def resolveInputFromWorld:Int
    def resolveOutputToWorld:Int
    def setWorldOutput(state:Boolean)
    def toggleWorldInput()

    override def onChange(gate:IOGateICPart)
    {
        val oldInput = gate.state&0xF
        val newInput = getInput(gate, ~(gate.state>>4)&inputMask(gate.shape))
        if (oldInput != newInput)
        {
            gate.setState(gate.state&0xF0|newInput)
            gate.onInputChange()
            gate.scheduleTick(1)
        }
    }

    override def scheduledTick(gate:IOGateICPart)
    {
        val oldOutput = gate.state>>4
        val newOutput = TICOrient.shiftMask(gate.state&0xF, 2)&outputMask(gate.shape)
        if (oldOutput != newOutput)
        {
            gate.setState(gate.state&0xF|newOutput<<4)
            gate.onOutputChange(oldOutput^newOutput)
        }
        onChange(gate)
    }

    @SideOnly(Side.CLIENT)
    def createGui(gate:IOGateICPart):CircuitGui = new ICIOGateGui(gate)

    override def getRolloverData(gate:IOGateICPart, detailLevel:Int) =
    {
        val s = Seq.newBuilder[String]
        if (detailLevel >= 2)
        {
            val f = getFreqName
            if (f.nonEmpty) s += "freq: "+f
            s += "mode: "+(gate.shape match
            {
                case 0 => "I"
                case 1 => "O"
                case 2 => "IO"
            })
        }
        if (detailLevel >= 3)
        {
            s += "I: "+(if (resolveInputFromWorld != 0) "high" else "low")
            s += "O: "+(if (resolveOutputToWorld != 0) "high" else "low")
        }
        super.getRolloverData(gate, detailLevel) ++ s.result().map(EnumChatFormatting.GRAY+_)
    }

    def getFreqName = ""

    override def activate(gate:IOGateICPart)
    {
        toggleWorldInput()
        gate.world.onInputChanged(1<<gate.rotation)
    }
}

trait TRSIOICGateLogic extends IOICGateLogic
{
    override def setup(gate:IOGateICPart)
    {
        if ((gate.world.iostate(gate.rotation)&0xFFFF) == 0)
        {
            gate.world.setInput(gate.rotation, 1)
            gate.world.onInputChanged(1<<gate.rotation)
        }
    }

    override def extInputChange(gate:IOGateICPart)
    {
        if ((gate.world.iostate(gate.rotation)&0xFFFF) == 0)
        {
            gate.world.setInput(gate.rotation, 1)
            gate.world.onInputChanged(1<<gate.rotation)
        }
        super.extInputChange(gate)
    }
}

class SimpleIOICGateLogic(gate:IOGateICPart) extends IOICGateLogic(gate) with TRSIOICGateLogic
{
    override def getConnMode(gate:IOGateICPart) = IIOCircuitPart.Simple

    override def resolveInputFromWorld =
        if ((gate.world.iostate(gate.rotation)&0x8000) != 0) 255
        else 0

    override def resolveOutputToWorld =
        if (((gate.world.iostate(gate.rotation)>>16)&0x8000) != 0) 255 else 0

    override def setWorldOutput(state:Boolean)
    {
        gate.world.setOutput(gate.rotation, if (state) 0x8000 else 1)
    }

    override def toggleWorldInput()
    {
        gate.world.setInput(gate.rotation, if ((gate.world.iostate(gate.rotation)&0x8000) != 0) 1 else 0x8000)
    }
}

trait TFreqIOICGateLogic extends IOICGateLogic
{
    var freq = 0

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setByte("freq", freq.toByte)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        freq = tag.getByte("freq")
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeByte(freq)
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        freq = in.readUByte()
    }

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 12 => freq = in.readUByte()
        case _ => super.read(in, key)
    }

    def sendFreqUpdate()
    {
        gate.writeStreamOf(12).writeByte(freq)
    }

    def freqUp()
    {
        if (freq < 15)
        {
            freq += 1
            sendFreqUpdate()
            onChange(gate)
        }
    }

    def freqDown()
    {
        if (freq > 0)
        {
            freq -= 1
            sendFreqUpdate()
            onChange(gate)
        }
    }

    override def resolveInputFromWorld =
        if ((gate.world.iostate(gate.rotation)&1<<freq) != 0) 255
        else 0

    override def resolveOutputToWorld =
        if ((gate.world.iostate(gate.rotation)>>>16&1<<freq) != 0) 255
        else 0

    override def setWorldOutput(state:Boolean)
    {
        val s = ((gate.world.iostate(gate.rotation)>>>16)& ~(1<<freq))|(if (state) 1 else 0)<<freq
        gate.world.setOutput(gate.rotation, s)
    }
}

class AnalogIOICGateLogic(gate:IOGateICPart) extends IOICGateLogic(gate) with TFreqIOICGateLogic with TRSIOICGateLogic
{
    override def getConnMode(gate:IOGateICPart) = IIOCircuitPart.Analog

    override def getFreqName = "0x"+freq

    override def toggleWorldInput()
    {
        val newInput = (gate.world.iostate(gate.rotation)&1<<freq)^1<<freq
        gate.world.setInput(gate.rotation, if (newInput == 0) 1 else newInput)
    }
}

class BundledIOICGateLogic(gate:IOGateICPart) extends IOICGateLogic(gate) with TFreqIOICGateLogic
{
    override def getConnMode(gate:IOGateICPart) = IIOCircuitPart.Bundled

    override def getFreqName = Colors(freq).name.toLowerCase

    override def toggleWorldInput()
    {
        gate.world.setInput(gate.rotation, (gate.world.iostate(gate.rotation)&0xFFFF)^1<<freq)
    }
}