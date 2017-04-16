/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.{MCDataInput, MCDataOutput}
import mrtjp.projectred.fabrication.SEIntegratedCircuit._
import net.minecraft.nbt.NBTTagCompound
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

trait IIOCircuitPart
{
    def getIOSide:Int
    def getIOMode:Int
    def getConnMode:Int
}

object IIOCircuitPart
{
    val Closed = 0
    val Input = 1
    val Output = 2
//    val InOut = 3

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

    override def getPartType = ICTileDefs.IOGate

//    override def onExtInputChanged(r:Int)
//    {
//        if (r == rotation) getLogicIO.extInputChange(this)
//    }
//    override def onExtOutputChanged(r:Int)
//    {
//        if (r == rotation) getLogicIO.extOutputChange(this)
//    }
    override def getIOSide = rotation
    override def getIOMode = getLogicIO.getIOMode(this)
    override def getConnMode = getLogicIO.getConnMode(this)

//    override def getRedstoneInput(r:Int):Int =
//    {
//        if (r == 0) getLogicIO.resolveInputFromWorld //r is to outside world
//        else super.getRedstoneInput(r)
//    }

//    override def onOutputChange(mask:Int)
//    {
//        super.onOutputChange(mask)
//        if ((mask&1) != 0)
//        {
//            val oldOutput = world.iostate(rotation)>>>16
//            getLogicIO.setWorldOutput((state&0x10) != 0)
//            val newOutput = world.iostate(rotation)>>>16
//            if (oldOutput != newOutput) world.onOutputChanged(1<<rotation)
//        }
//    }

    def getSysInputRegister(freq:Int):Int = REG_IN(getIOSide, freq)
    def getSysOutputRegister(freq:Int):Int = REG_OUT(getIOSide, freq)
}

object IOICGateLogic
{
    import mrtjp.projectred.fabrication.{ICGateDefinition => defs}

    def create(gate:IOGateICPart, subID:Int) = subID match
    {
        case defs.IOSimple.ordinal => new SimpleIOICGateLogic(gate)
        case defs.IOAnalog.ordinal => new AnalogIOICGateLogic(gate)
        case defs.IOBundled.ordinal => new BundledIOICGateLogic(gate)
        case _ => throw new IllegalArgumentException("Invalid gate subID: "+subID)
    }
}

abstract class IOICGateLogic(val gate:IOGateICPart) extends RedstoneICGateLogic[IOGateICPart] with TComplexICGateLogic[IOGateICPart]
{
    import IIOCircuitPart._

//    val inputRegs = Array(REG_ZERO, REG_ZERO, REG_ZERO, REG_ZERO)
//    val outputRegs = Array(REG_ZERO, REG_ZERO, REG_ZERO, REG_ZERO)
    var inputReg = REG_ZERO
    var outputReg = REG_ZERO

    override def inputMask(shape:Int) = shape match
    {
        case 0 => 1
        case 1 => 4
//        case 2 => 5
    }
    override def outputMask(shape:Int) = shape match
    {
        case 0 => 4
        case 1 => 1
//        case 2 => 5
    }

    override def cycleShape(gate:IOGateICPart) =
    {
        gate.setShape((gate.shape+1)%2)
        true
    }

//    def extInputChange(gate:IOGateICPart){gate.onSchematicChanged()}
//    def extOutputChange(gate:IOGateICPart){}
    def getIOMode(gate:IOGateICPart):Int = gate.shape match {
        case 0 => Input
        case 1 => Output
//        case 2 => InOut
    }

    def getConnMode(gate:IOGateICPart):Int

    def getFreq:Int

    def getFreqName:String

    def enforceBit = false

//    def resolveInputFromWorld:Int
//    def resolveOutputToWorld:Int
//    def setWorldOutput(state:Boolean)
    def toggleWorldInput()
    {
        val newInput = (gate.editor.simEngineContainer.iostate(gate.rotation)&1<<getFreq)^1<<getFreq
        gate.editor.simEngineContainer.setInput(gate.rotation, if (newInput == 0 && enforceBit) 1 else newInput)
    }

    override def onRegistersChanged(gate:IOGateICPart, regIDs:Set[Int])
    {
        val oldState = gate.state
        val newState = pullInput(gate)&0xF | pullOutput(gate)<<4
        if (oldState != newState) {
            gate.setState(newState)
            gate.sendStateUpdate()
        }
    }

    def pullInput(gate:IOGateICPart) = //Pull the input from the sim engine
    {
        if (getIOMode(gate) == Input && gate.editor.simEngineContainer.simEngine.getRegVal[Byte](inputReg) != 0)
            4 else 0

//        if (gate.world.simEngineContainer.simEngine.getRegVal[Byte](inputReg) != 0)
//            inputMask(gate.shape) else 0
    }

    def pullOutput(gate:IOGateICPart) = //Pull the output form the sim engine
    {
        if (getIOMode(gate) == Output && gate.editor.simEngineContainer.simEngine.getRegVal[Byte](outputReg) != 0)
            4 else 0

//        if (gate.world.simEngineContainer.simEngine.getRegVal[Byte](outputReg) != 0)
//            outputMask(gate.shape) else 0
    }

    override def allocateOrFindRegisters(gate:IOGateICPart, linker:ISELinker)
    {
        getIOMode(gate) match {
            case Input => //From world to simulation
                inputReg = gate.getSysInputRegister(getFreq)
                outputReg = gate.getOutputRegister(2, linker)
            case Output => //from simulation to world
                inputReg = gate.getInputRegister(2, linker)
                outputReg = gate.getSysOutputRegister(getFreq)
        }
    }

    override def declareOperations(gate:IOGateICPart, linker:ISELinker)
    {
        val comp = getOutputOp(inputReg, outputReg)
        linker.addGate(linker.allocateGateID(), comp, Seq(inputReg), Seq(outputReg))
    }

    def getOutputOp(input:Int, output:Int):ISEGate =
    {
        new ISEGate {
            override def compute(ic:SEIntegratedCircuit) {
                ic.queueRegVal[Byte](output, if (ic.getRegVal(input) != 0) 1 else 0)
            }
        }
    }

    @SideOnly(Side.CLIENT)
    override def getRolloverData(gate:IOGateICPart, detailLevel:Int) =
    {
        val s = Seq.newBuilder[String]
        if (detailLevel >= 2) {
            val f = getFreqName
            if (f.nonEmpty) s += "freq: "+f
            s += "mode: "+(gate.shape match {
                case 0 => "I"
                case 1 => "O"
//                case 2 => "IO"
            })
        }
        if (detailLevel >= 3) {
            s += "I: "+(if ((gate.state&0xF) != 0) "high" else "low")
            s += "O: "+(if ((gate.state>>4) != 0) "high" else "low")
        }
        super.getRolloverData(gate, detailLevel) ++ s.result()
    }

    override def activate(gate:IOGateICPart)
    {
        toggleWorldInput()
        gate.editor.simEngineContainer.onInputChanged(1<<gate.rotation)
    }
}

//trait TRSIOICGateLogic extends IOICGateLogic
//{
//    override def setup(gate:IOGateICPart)
//    {
//        if ((gate.world.simEngineContainer.iostate(gate.rotation)&0xFFFF) == 0)
//        {
//            gate.world.simEngineContainer.setInput(gate.rotation, 1)
//            gate.world.simEngineContainer.onInputChanged(1<<gate.rotation)
//        }
//    }

//    override def extInputChange(gate:IOGateICPart)
//    {
//        if ((gate.world.simEngineContainer.iostate(gate.rotation)&0xFFFF) == 0)
//        {
//            gate.world.simEngineContainer.setInput(gate.rotation, 1)
//            gate.world.simEngineContainer.onInputChanged(1<<gate.rotation)
//        }
//        super.extInputChange(gate)
//    }
//}

class SimpleIOICGateLogic(gate:IOGateICPart) extends IOICGateLogic(gate) //with TRSIOICGateLogic
{
    override def getConnMode(gate:IOGateICPart) = IIOCircuitPart.Simple

    override def getFreq = 0

    override def getFreqName = "rs_gpio"

    //    override def resolveInputFromWorld =
//        if ((gate.world.simEngineContainer.iostate(gate.rotation)&0xFFFE) != 0) 255
//        else 0
//
//    override def resolveOutputToWorld =
//        if (((gate.world.simEngineContainer.iostate(gate.rotation)>>16)&0xFFFE) != 0) 255 else 0

//    override def setWorldOutput(state:Boolean)
//    {
//        gate.world.simEngineContainer.setOutput(gate.rotation, if (state) 0x8000 else 1)
//    }

    @SideOnly(Side.CLIENT)
    override def createGui(gate:IOGateICPart):CircuitGui = new ICIOGateGui(gate)
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
        if (freq < 15) {
            freq += 1
            sendFreqUpdate()
            gate.onSchematicChanged()
        }
    }

    def freqDown()
    {
        if (freq > 0) {
            freq -= 1
            sendFreqUpdate()
            gate.onSchematicChanged()
        }
    }

    override def getFreq = freq

//    override def resolveInputFromWorld =
//        if ((gate.world.simEngineContainer.iostate(gate.rotation)&1<<freq) != 0) 255
//        else 0
//
//    override def resolveOutputToWorld =
//        if ((gate.world.simEngineContainer.iostate(gate.rotation)>>>16&1<<freq) != 0) 255
//        else 0

//    override def setWorldOutput(state:Boolean)
//    {
//        val s = ((gate.world.simEngineContainer.iostate(gate.rotation)>>>16)& ~(1<<freq))|(if (state) 1 else 0)<<freq
//        gate.world.simEngineContainer.setOutput(gate.rotation, s)
//    }

    @SideOnly(Side.CLIENT)
    override def createGui(gate:IOGateICPart):CircuitGui = new ICIOFreqGateGui(gate)
}

class AnalogIOICGateLogic(gate:IOGateICPart) extends IOICGateLogic(gate) with TFreqIOICGateLogic// with TRSIOICGateLogic
{
    override def getConnMode(gate:IOGateICPart) = IIOCircuitPart.Analog

    override def getFreqName = "0x"+Integer.toHexString(freq)

    override def enforceBit = true

//    override def toggleWorldInput()
//    {
//        val newInput = (gate.world.simEngineContainer.iostate(gate.rotation)&1<<freq)^1<<freq
//        gate.world.simEngineContainer.setInput(gate.rotation, if (newInput == 0) 1 else newInput)
//    }
}

class BundledIOICGateLogic(gate:IOGateICPart) extends IOICGateLogic(gate) with TFreqIOICGateLogic
{
    override def getConnMode(gate:IOGateICPart) = IIOCircuitPart.Bundled

    override def getFreqName = EnumColour.values()(freq).name.toLowerCase

//    override def toggleWorldInput()
//    {
//        gate.world.simEngineContainer.setInput(gate.rotation,
//            (gate.world.simEngineContainer.iostate(gate.rotation)&0xFFFF)^1<<freq)
//    }
}