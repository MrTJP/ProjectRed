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

    val NoConn = 0
    val Simple = 1
    val Analog = 2
    val Bundled = 3
}

class IOGateICTile extends RedstoneGateICTile with IIOCircuitPart with TComplexGateICTile
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

    override def getIOSide = rotation
    override def getIOMode = getLogicIO.getIOMode(this)
    override def getConnMode = getLogicIO.getConnMode(this)

    def getSysInputRegister(freq:Int):Int = REG_IN(getIOSide, freq)
    def getSysOutputRegister(freq:Int):Int = REG_OUT(getIOSide, freq)
}

object IOICGateLogic
{
    import mrtjp.projectred.fabrication.{ICGateDefinition => defs}

    def create(gate:IOGateICTile, subID:Int) = subID match
    {
        case defs.IOSimple.ordinal => new SimpleIOICGateLogic(gate)
        case defs.IOAnalog.ordinal => new AnalogIOICGateLogic(gate)
        case defs.IOBundled.ordinal => new BundledIOICGateLogic(gate)
        case _ => throw new IllegalArgumentException("Invalid gate subID: "+subID)
    }
}

abstract class IOICGateLogic(val gate:IOGateICTile) extends RedstoneICGateLogic[IOGateICTile] with TComplexICGateLogic[IOGateICTile]
{
    import IIOCircuitPart._

    var inputReg = REG_ZERO
    var outputReg = REG_ZERO

    override def inputMask(shape:Int) = shape match
    {
        case 0 => 1
        case 1 => 4
    }
    override def outputMask(shape:Int) = shape match
    {
        case 0 => 4
        case 1 => 1
    }

    override def cycleShape(gate:IOGateICTile) =
    {
        gate.setShape((gate.shape+1)%2)
        true
    }

    def getIOMode(gate:IOGateICTile):Int = gate.shape match {
        case 0 => Input
        case 1 => Output
    }

    def getConnMode(gate:IOGateICTile):Int

    def getFreq:Int

    def getFreqName:String

    def enforceBit = false

    def toggleWorldInput()
    {
        val newInput = (gate.editor.simEngineContainer.iostate(gate.rotation)&1<<getFreq)^1<<getFreq
        gate.editor.simEngineContainer.setInput(gate.rotation, if (newInput == 0 && enforceBit) 1 else newInput)
    }

    override def onRegistersChanged(gate:IOGateICTile, regIDs:Set[Int])
    {
        val oldState = gate.state
        val newState = pullInput(gate)&0xF | pullOutput(gate)<<4
        if (oldState != newState) {
            gate.setState(newState)
            gate.sendStateUpdate()
        }
    }

    def pullInput(gate:IOGateICTile) = //Pull the input from the sim engine
    {
        if (getIOMode(gate) == Input && gate.editor.simEngineContainer.simEngine.getRegVal[Byte](inputReg) != 0)
            4 else 0
    }

    def pullOutput(gate:IOGateICTile) = //Pull the output form the sim engine
    {
        if (getIOMode(gate) == Output && gate.editor.simEngineContainer.simEngine.getRegVal[Byte](outputReg) != 0)
            4 else 0
    }

    override def allocateOrFindRegisters(gate:IOGateICTile, linker:ISELinker)
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

    override def declareOperations(gate:IOGateICTile, linker:ISELinker)
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
    override def getRolloverData(gate:IOGateICTile, detailLevel:Int) =
    {
        val s = Seq.newBuilder[String]
        if (detailLevel >= 2) {
            val f = getFreqName
            if (f.nonEmpty) s += "freq: "+f
            s += "mode: "+(gate.shape match {
                case 0 => "I"
                case 1 => "O"
            })
        }
        if (detailLevel >= 3) {
            s += "I: "+(if ((gate.state&0xF) != 0) "high" else "low")
            s += "O: "+(if ((gate.state>>4) != 0) "high" else "low")
        }
        super.getRolloverData(gate, detailLevel) ++ s.result()
    }

    override def activate(gate:IOGateICTile)
    {
        toggleWorldInput()
        gate.editor.simEngineContainer.onInputChanged(1<<gate.rotation)
    }
}

class SimpleIOICGateLogic(gate:IOGateICTile) extends IOICGateLogic(gate)
{
    override def getConnMode(gate:IOGateICTile) = IIOCircuitPart.Simple

    override def getFreq = 0

    override def getFreqName = "rs_gpio"

    @SideOnly(Side.CLIENT)
    override def createGui(gate:IOGateICTile):CircuitGui = new ICIOGateGui(gate)
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

    @SideOnly(Side.CLIENT)
    override def createGui(gate:IOGateICTile):CircuitGui = new ICIOFreqGateGui(gate)
}

class AnalogIOICGateLogic(gate:IOGateICTile) extends IOICGateLogic(gate) with TFreqIOICGateLogic
{
    override def getConnMode(gate:IOGateICTile) = IIOCircuitPart.Analog

    override def getFreqName = "0x"+Integer.toHexString(freq)

    override def enforceBit = true
}

class BundledIOICGateLogic(gate:IOGateICTile) extends IOICGateLogic(gate) with TFreqIOICGateLogic
{
    override def getConnMode(gate:IOGateICTile) = IIOCircuitPart.Bundled

    override def getFreqName = EnumColour.values()(freq).name.toLowerCase
}