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

import scala.collection.mutable.ListBuffer

trait IIOGateTile
{
    def getIOSide:Int
    def getIOMode:Int
    def getConnMode:Int
}

object IIOGateTile
{
    val Closed = 0
    val Input = 1
    val Output = 2

    val NoConn = 0
    val Simple = 1
    val Analog = 2
    val Bundled = 3
}

class IOGateICTile extends RedstoneGateICTile with IIOGateTile with TComplexGateICTile
{
    private var logic:IOGateTileLogic = null

    override def getLogic[T] = logic.asInstanceOf[T]
    def getLogicIO = getLogic[IOGateTileLogic]

    override def assertLogic()
    {
        if (logic == null) logic = IOGateTileLogic.create(this, subID)
    }

    override def readClientPacket(in:MCDataInput, key:Int) = key match
    {
        case 5 => getLogicIO match {
            case f:TFreqIOGateTileLogic => f.freqUp()
            case _ =>
        }
        case 6 => getLogicIO match {
            case f:TFreqIOGateTileLogic => f.freqDown()
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

object IOGateTileLogic
{
    import mrtjp.projectred.fabrication.{ICGateDefinition => defs}

    def create(gate:IOGateICTile, subID:Int) = subID match
    {
        case defs.IOSimple.ordinal => new SimpleIOGateTileLogic(gate)
        case defs.IOAnalog.ordinal => new AnalogIOGateTileLogic(gate)
        case defs.IOBundled.ordinal => new BundledIOGateTileLogic(gate)
        case _ => throw new IllegalArgumentException("Invalid gate subID: "+subID)
    }
}

abstract class IOGateTileLogic(val gate:IOGateICTile) extends RedstoneGateTileLogic[IOGateICTile] with TComplexGateTileLogic[IOGateICTile]
{
    import IIOGateTile._

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

    def getInputRegisterOffset:Int

    def getFreqName:String

    def toggleWorldInput()

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
                inputReg = gate.getSysInputRegister(getInputRegisterOffset)
                outputReg = gate.getOutputRegister(2, linker)
            case Output => //from simulation to world
                inputReg = gate.getInputRegister(2, linker)
                outputReg = gate.getSysOutputRegister(getInputRegisterOffset)
        }
    }

    override def declareOperations(gate:IOGateICTile, linker:ISELinker)
    {
        val comp = getOutputOp(inputReg, outputReg)
        linker.addGate(linker.allocateGateID(Set(gate.pos)), comp, Seq(inputReg), Seq(outputReg))
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
    override def buildRolloverData(gate:IOGateICTile, buffer:ListBuffer[String]) =
    {
        super.buildRolloverData(gate, buffer)
        import com.mojang.realmsclient.gui.ChatFormatting._
        buffer += GRAY + "freq: "+getFreqName
        buffer += GRAY + "mode: "+(gate.shape match {
            case 0 => "I"
            case 1 => "O"
        })

        if (gate.getIOMode == Input)
            buffer += GRAY + "I: "+(if ((gate.state&0xF) != 0) "high" else "low")
        else
            buffer += GRAY + "O: "+(if ((gate.state>>4) != 0) "high" else "low")
    }

    override def activate(gate:IOGateICTile)
    {
        toggleWorldInput()
        gate.editor.simEngineContainer.onInputChanged(1<<gate.rotation)
        gate.editor.simEngineContainer.repropagate()
    }
}

class SimpleIOGateTileLogic(gate:IOGateICTile) extends IOGateTileLogic(gate)
{
    override def getConnMode(gate:IOGateICTile) = IIOGateTile.Simple

    override def getInputRegisterOffset = 0

    override def getFreqName = "rs_gpio"

    override def toggleWorldInput()
    {
        gate.editor.simEngineContainer.setInput(gate.rotation,
            (gate.editor.simEngineContainer.iostate(gate.rotation)&0xFFFF)^1)
    }

    @SideOnly(Side.CLIENT)
    override def createGui(gate:IOGateICTile):ICTileGui = new ICIOGateGui(gate)
}

trait TFreqIOGateTileLogic extends IOGateTileLogic
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

    override def getInputRegisterOffset = freq

    override def onGatePlaced(gate:IOGateICTile)
    {
        super.onGatePlaced(gate)
        val ioParts = gate.editor.tileMapContainer.tiles.collect {
            case (pos, io:IOGateICTile)
                if io.getConnMode == gate.getConnMode &&
                        gate.getIOSide == io.getIOSide &&
                        gate != io => io
        }

        if (ioParts.nonEmpty) {
            val largestFreq = ioParts.map(_.getLogic[TFreqIOGateTileLogic].freq).max
            if (largestFreq < 15) {
                freq = largestFreq + 1
                sendFreqUpdate()
                gate.onSchematicChanged()
            }
        }
    }

    @SideOnly(Side.CLIENT)
    override def createGui(gate:IOGateICTile):ICTileGui = new ICIOFreqGateGui(gate)
}

class AnalogIOGateTileLogic(gate:IOGateICTile) extends IOGateTileLogic(gate) with TFreqIOGateTileLogic
{
    override def getConnMode(gate:IOGateICTile) = IIOGateTile.Analog

    override def getFreqName = "0x"+Integer.toHexString(freq)

    override def toggleWorldInput()
    {
        val newInput = (gate.editor.simEngineContainer.iostate(gate.rotation)&1<<freq)^1<<freq
        gate.editor.simEngineContainer.setInput(gate.rotation, if (newInput == 0) 1 else newInput)
    }
}

class BundledIOGateTileLogic(gate:IOGateICTile) extends IOGateTileLogic(gate) with TFreqIOGateTileLogic
{
    override def getConnMode(gate:IOGateICTile) = IIOGateTile.Bundled

    override def getFreqName = EnumColour.values()(freq).name.toLowerCase

    override def toggleWorldInput()
    {
        gate.editor.simEngineContainer.setInput(gate.rotation,
            (gate.editor.simEngineContainer.iostate(gate.rotation)&0xFFFF)^1<<freq)
    }
}