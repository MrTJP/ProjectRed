/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import com.mojang.realmsclient.gui.ChatFormatting.GRAY
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

import scala.collection.mutable.ListBuffer

class ComboGateICTile extends RedstoneGateICTile
{
    val inputRegs = Array(-1, -1, -1, -1)
    val outputRegs = Array(-1, -1, -1, -1)

    override def getLogic[T] = ComboGateTileLogic.instances(subID).asInstanceOf[T]
    def getLogicCombo = getLogic[ComboGateTileLogic]

    override def getPartType = ICTileDefs.SimpleGate
}

object ComboGateTileLogic
{
    val advanceDead = Seq(1, 2, 4, 0, 5, 6, 3)

    val instances = new Array[ComboGateTileLogic](ICGateDefinition.values.length)
    initialize()

    def initialize()
    {
        import mrtjp.projectred.fabrication.{ICGateDefinition => defs}
        instances(defs.OR.ordinal) = OR
        instances(defs.NOR.ordinal) = NOR
        instances(defs.NOT.ordinal) = NOT
        instances(defs.AND.ordinal) = AND
        instances(defs.NAND.ordinal) = NAND
        instances(defs.XOR.ordinal) = XOR
        instances(defs.XNOR.ordinal) = XNOR
        instances(defs.Buffer.ordinal) = Buffer
//        instances(defs.Multiplexer.ordinal) = Multiplexer
//        instances(defs.Pulse.ordinal) = Pulse
//        instances(defs.Repeater.ordinal) = Repeater
//        instances(defs.Randomizer.ordinal) = Randomizer
//        instances(defs.TransparentLatch.ordinal) = TransparentLatch
//        instances(defs.DecRandomizer.ordinal) = DecRandomizer
    }
}

trait TSimpleRSGateTileLogic[T <: RedstoneGateICTile] extends RedstoneGateTileLogic[T]
{
    def getDelay(shape:Int) = 0

    @SideOnly(Side.CLIENT)
    override def buildRolloverData(gate:T, buffer:ListBuffer[String])
    {
        super.buildRolloverData(gate, buffer)
        buffer += GRAY + "I: "+rolloverInput(gate)
        buffer += GRAY + "O: "+rolloverOutput(gate)
    }

    def rolloverInput(gate:T) = "0x"+Integer.toHexString(gate.state&0xF)
    def rolloverOutput(gate:T) = "0x"+Integer.toHexString(gate.state>>4)
}

abstract class ComboGateTileLogic extends RedstoneGateTileLogic[ComboGateICTile] with TSimpleRSGateTileLogic[ComboGateICTile]
{
    override def cycleShape(gate:ComboGateICTile) =
    {
        val oldShape = gate.shape
        val newShape = cycleShape(oldShape)
        if (newShape != oldShape) {
            gate.setShape(newShape)
            true
        }
        else false
    }

    def cycleShape(shape:Int):Int =
    {
        if (deadSides == 0) return shape

        var shape1 = shape
        import java.lang.Integer.{bitCount, numberOfLeadingZeros => lead}
        do shape1 = ComboGateTileLogic.advanceDead(shape1)
        while (bitCount(shape1) > maxDeadSides || 32-lead(shape1) > deadSides)
        shape1
    }

    def deadSides = 0
    def maxDeadSides = deadSides-1

    def pullInput(gate:ComboGateICTile, mask:Int) = //Pull the input from the sim engine
    {
        var input = 0
        for (r <- 0 until 4) if ((mask&1<<r) != 0) {
            if (gate.editor.simEngineContainer.simEngine.getRegVal[Byte](gate.inputRegs(r)) > 0) input |= 1<<r
        }
        input
    }

    def pullOutput(gate:ComboGateICTile, mask:Int) = //Pull the output form the sim engine
    {
        var output = 0
        for (r <- 0 until 4) if ((mask&1<<r) != 0) {
            if (gate.editor.simEngineContainer.simEngine.getRegVal[Byte](gate.outputRegs(r)) > 0) output |= 1<<r
        }
        output
    }

    override def onRegistersChanged(gate:ComboGateICTile, regIDs:Set[Int]) //Use to set state on gates/update render
    {
        val oldState = gate.state
        val newState = pullInput(gate, inputMask(gate.shape))&0xF | pullOutput(gate, outputMask(gate.shape))<<4
        if (oldState != newState) {
            gate.setState(newState)
            gate.sendStateUpdate()
        }
    }

    override def allocateOrFindRegisters(gate:ComboGateICTile, linker:ISELinker)
    {
        for (r <- 0 until 4) {
            gate.inputRegs(r) =
                    if (canInput(gate, r)) gate.getInputRegister(r, linker) else -1
            gate.outputRegs(r) =
                    if (canOutput(gate, r)) gate.getOutputRegister(r, linker) else -1
        }
    }

    override def declareOperations(gate:ComboGateICTile, linker:ISELinker)
    {
        val comp = getOutputOp(gate.inputRegs, gate.outputRegs)
        linker.addGate(linker.allocateGateID(), comp,
            gate.inputRegs.filter(_ != -1).toSeq.distinct,
            gate.outputRegs.filter(_ != -1).toSeq.distinct)
    }

    def getOutputOp(inputs:Array[Int], outputs:Array[Int]):ISEGate
}

object OR extends ComboGateTileLogic
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = ~shape<<1&0xE

    override def deadSides = 3

    override def getOutputOp(inputs:Array[Int], outputs:Array[Int]) =
    {
        val inIDs = inputs.filter(_ != -1).toSeq
        val outID = outputs(0)

        new ISEGate {
            override def compute(ic:SEIntegratedCircuit) {
                ic.queueRegVal[Byte](outID, if (inIDs.exists(ic.getRegVal(_) != 0)) 1 else 0)
            }
        }
    }
}

object NOR extends ComboGateTileLogic
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = ~shape<<1&0xE

    override def deadSides = 3

    override def getOutputOp(inputs:Array[Int], outputs:Array[Int]) =
    {
        val inIDs = inputs.filter(_ != -1).toSeq
        val outID = outputs(0)

        new ISEGate {
            override def compute(ic:SEIntegratedCircuit) {
                ic.queueRegVal[Byte](outID, if (inIDs.exists(ic.getRegVal(_) != 0)) 0 else 1)
            }
        }
    }
}

object NOT extends ComboGateTileLogic
{
    override def outputMask(shape:Int) = ~((shape&1)<<1|(shape&2)>>1|(shape&4)<<1)&0xB
    override def inputMask(shape:Int) = 4

    override def deadSides = 3

    override def getOutputOp(inputs:Array[Int], outputs:Array[Int]) =
    {
        val inID = inputs(2)
        val outIDs = outputs.filter(_ != -1).toSeq

        new ISEGate {
            override def compute(ic:SEIntegratedCircuit) {
                val outVal = (if (ic.getRegVal(inID) != 0) 0 else 1).toByte
                outIDs.foreach(ic.queueRegVal[Byte](_, outVal))
            }
        }
    }
}

object AND extends ComboGateTileLogic
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = ~shape<<1&0xE

    override def deadSides = 3

    override def getOutputOp(inputs:Array[Int], outputs:Array[Int]) =
    {
        val inIDs = inputs.filter(_ != -1).toSeq
        val outID = outputs(0)

        new ISEGate {
            override def compute(ic:SEIntegratedCircuit) {
                ic.queueRegVal[Byte](outID, if (inIDs.forall(ic.getRegVal(_) != 0)) 1 else 0)
            }
        }
    }
}

object NAND extends ComboGateTileLogic
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = ~shape<<1&0xE

    override def deadSides = 3

    override def getOutputOp(inputs:Array[Int], outputs:Array[Int]) =
    {
        val inIDs = inputs.filter(_ != -1).toSeq
        val outID = outputs(0)

        new ISEGate {
            override def compute(ic:SEIntegratedCircuit) {
                ic.queueRegVal[Byte](outID, if (inIDs.forall(ic.getRegVal(_) != 0)) 0 else 1)
            }
        }
    }
}

object XOR extends ComboGateTileLogic
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = 10

    override def getOutputOp(inputs:Array[Int], outputs:Array[Int]) =
    {
        val in1 = inputs(1)
        val in2 = inputs(3)
        val outID = outputs(0)

        new ISEGate {
            override def compute(ic:SEIntegratedCircuit) {
                ic.queueRegVal[Byte](outID, if (ic.getRegVal(in1) != ic.getRegVal(in2)) 1 else 0)
            }
        }
    }

}

object XNOR extends ComboGateTileLogic
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = 10

    override def getOutputOp(inputs:Array[Int], outputs:Array[Int]) =
    {
        val in1 = inputs(1)
        val in2 = inputs(3)
        val outID = outputs(0)

        new ISEGate {
            override def compute(ic:SEIntegratedCircuit) {
                ic.queueRegVal[Byte](outID, if (ic.getRegVal(in1) == ic.getRegVal(in2)) 1 else 0)
            }
        }
    }
}

object Buffer extends ComboGateTileLogic
{
    override def outputMask(shape:Int) = ~((shape&1)<<1|(shape&2)<<2)&0xB
    override def inputMask(shape:Int) = 4

    override def deadSides = 2
    override def maxDeadSides = 2

    override def getOutputOp(inputs:Array[Int], outputs:Array[Int]) =
    {
        val inID = inputs(2)
        val outIDs = outputs.filter(_ != -1).toSeq

        new ISEGate {
            override def compute(ic:SEIntegratedCircuit) {
                val in = ic.getRegVal[Byte](inID)
                outIDs.foreach(ic.queueRegVal[Byte](_, in))
            }
        }
    }
}

//object Multiplexer extends ComboICGateLogic
//{
//    override def outputMask(shape:Int) = 1
//    override def inputMask(shape:Int) = 0xE
//
//    override def calcOutput(gate:ComboGateICPart, input:Int) = if ((input&1<<2) != 0) (input>>3)&1 else (input>>1)&1
//}
//
//object Pulse extends ComboICGateLogic
//{
//    override def outputMask(shape:Int) = 1
//    override def inputMask(shape:Int) = 4
//
//    override def calcOutput(gate:ComboGateICPart, input:Int) = 0
//
//    override def onChange(gate:ComboGateICPart) =
//    {
//        val oldInput = gate.state&0xF
//        val newInput = getInput(gate, 4)
//
//        if (oldInput != newInput)
//        {
//            gate.setState(gate.state&0xF0|newInput)
//            gate.onInputChange()
//            if (newInput != 0 && (gate.state&0xF0) == 0)
//            {
//                gate.setState(gate.state&0xF|0x10)
//                gate.scheduleTick(2)
//                gate.onOutputChange(1)
//            }
//        }
//    }
//}
//
//object Repeater extends ComboICGateLogic
//{
//    val delays = Array(2, 4, 6, 8, 16, 32, 64, 128, 256)
//
//    override def outputMask(shape:Int) = 1
//    override def inputMask(shape:Int) = 4
//
//    override def getDelay(shape:Int) = delays(shape)
//
//    override def cycleShape(shape:Int) = (shape+1)%delays.length
//
//    override def calcOutput(gate:ComboGateICPart, input:Int) = if (input == 0) 0 else 1
//
//    override def onChange(gate:ComboGateICPart){ if (gate.schedTime < 0) super.onChange(gate) }
//
//    override def activate(gate:ComboGateICPart)
//    {
//        gate.configure()
//    }
//
//    @SideOnly(Side.CLIENT)
//    override def getRolloverData(gate:ComboGateICPart, detailLevel:Int) =
//    {
//        val data = Seq.newBuilder[String]
//        if (detailLevel > 1) data += "delay: "+delays(gate.shape)
//        super.getRolloverData(gate, detailLevel)++data.result()
//    }
//}
//
//object Randomizer extends ComboICGateLogic
//{
//    val rand = new Random
//
//    override def outputMask(shape:Int) = ~((shape&1)<<1|(shape&2)>>1|(shape&4)<<1)&0xB
//    override def inputMask(shape:Int) = 4
//    override def feedbackMask(shape:Int) = outputMask(shape)
//
//    override def deadSides = 3
//
//    override def getDelay(shape:Int) = 2
//
//    override def calcOutput(gate:ComboGateICPart, input:Int) =
//    {
//        if (input == 0) gate.state>>4 else
//            outputMask(gate.shape)&TFaceOrient.shiftMask(rand.nextInt(8), 3)
//    }
//
//    override def onChange(gate:ComboGateICPart)
//    {
//        super.onChange(gate)
//        if ((gate.state&4) != 0) gate.scheduleTick(2)
//    }
//}
//
//object TransparentLatch extends ComboICGateLogic
//{
//    override def outputMask(shape:Int) = if (shape == 0) 3 else 9
//    override def inputMask(shape:Int) = if (shape == 0) 0xC else 6
//
//    override def cycleShape(shape:Int) = shape^1
//
//    override def calcOutput(gate:ComboGateICPart, input:Int) =
//    {
//        if ((input&4) == 0) gate.state>>4
//        else if ((input&0xA) == 0) 0 else 0xF
//    }
//}
//
//object DecRandomizer extends ComboICGateLogic
//{
//    val rand = new Random
//
//    override def cycleShape(shape:Int) = shape^1
//
//    override def outputMask(shape:Int) = if (shape == 0) 11 else 9
//    override def inputMask(shape:Int) = 4
//    override def feedbackMask(shape:Int) = 2
//
//    override def getDelay(shape:Int) = 2
//
//    override def calcOutput(gate:ComboGateICPart, input:Int) =
//    {
//        if (input == 0) if ((gate.state>>4) == 0) 1 else gate.state>>4
//        else Seq(1, 8, 2)(rand.nextInt((~gate.shape|2)&3))
//    }
//
//    override def onChange(gate:ComboGateICPart)
//    {
//        super.onChange(gate)
//        if ((gate.state&4) != 0) gate.scheduleTick(2)
//    }
//}