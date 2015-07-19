/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import java.util.Random

import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.projectred.core.TFaceOrient

class ComboGateICPart extends RedstoneGateICPart
{
    override def getLogic[T] = ComboICGateLogic.instances(subID).asInstanceOf[T]
    def getLogicCombo = getLogic[ComboICGateLogic]

    override def getPartType = CircuitPartDefs.SimpleGate
}

object ComboICGateLogic
{
    val advanceDead = Seq(1, 2, 4, 0, 5, 6, 3)

    val instances = new Array[ComboICGateLogic](ICGateDefinition.values.length)
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
        instances(defs.Multiplexer.ordinal) = Multiplexer
        instances(defs.Pulse.ordinal) = Pulse
        instances(defs.Repeater.ordinal) = Repeater
        instances(defs.Randomizer.ordinal) = Randomizer
        instances(defs.TransparentLatch.ordinal) = TransparentLatch
        instances(defs.DecRandomizer.ordinal) = DecRandomizer
    }
}

trait TSimpleRSICGateLogic[T <: RedstoneGateICPart] extends RedstoneICGateLogic[T]
{
    def getDelay(shape:Int) = 0

    def feedbackMask(shape:Int) = 0

    def calcOutput(gate:T, input:Int) = 0

    override def onChange(gate:T)
    {
        val iMask = inputMask(gate.shape)
        val oMask = outputMask(gate.shape)
        val fMask = feedbackMask(gate.shape)
        val oldInput = gate.state&0xF
        val newInput = getInput(gate, iMask|fMask)
        if (oldInput != newInput)
        {
            gate.setState(gate.state&0xF0|newInput)
            gate.onInputChange()
        }

        val newOutput = calcOutput(gate, gate.state&iMask)&oMask
        if (newOutput != (gate.state>>4)) gate.scheduleTick(getDelay(gate.shape))
    }

    override def scheduledTick(gate:T)
    {
        val iMask = inputMask(gate.shape)
        val oMask = outputMask(gate.shape)
        val oldOutput = gate.state>>4
        val newOutput = calcOutput(gate, gate.state&iMask)&oMask
        if (oldOutput != newOutput)
        {
            gate.setState(gate.state&0xF|newOutput<<4)
            gate.onOutputChange(oMask)
        }
        onChange(gate)
    }

    override def setup(gate:T)
    {
        val iMask = inputMask(gate.shape)
        val oMask = outputMask(gate.shape)
        val output = calcOutput(gate, getInput(gate, iMask))&oMask
        if (output != 0)
        {
            gate.setState(output<<4)
            gate.onOutputChange(output) //use output for change mask because nothing is going low
        }
    }

    @SideOnly(Side.CLIENT)
    override def getRolloverData(gate:T, detailLevel:Int) =
    {
        val s = Seq.newBuilder[String]
        if (detailLevel > 2) s += "I: "+rolloverInput(gate) += "O: "+rolloverOutput(gate)
        super.getRolloverData(gate, detailLevel) ++ s.result()
    }
    def rolloverInput(gate:T) = "0x"+Integer.toHexString(gate.state&0xF)
    def rolloverOutput(gate:T) = "0x"+Integer.toHexString(gate.state>>4)
}

abstract class ComboICGateLogic extends RedstoneICGateLogic[ComboGateICPart] with TSimpleRSICGateLogic[ComboGateICPart]
{
    override def cycleShape(gate:ComboGateICPart) =
    {
        val oldShape = gate.shape
        val newShape = cycleShape(oldShape)
        if (newShape != oldShape)
        {
            gate.setShape(newShape)
            true
        }
        else false
    }

    def cycleShape(shape:Int) =
    {
        var shape1 = shape
        import java.lang.Integer.{bitCount, numberOfLeadingZeros => lead}
        do shape1 = ComboICGateLogic.advanceDead(shape1)
        while (bitCount(shape1) > maxDeadSides || 32-lead(shape1) > deadSides)
        shape1
    }

    def deadSides = 0
    def maxDeadSides = deadSides-1
}

object OR extends ComboICGateLogic
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = ~shape<<1&0xE

    override def deadSides = 3

    override def calcOutput(gate:ComboGateICPart, input:Int) = if (input != 0) 1 else 0
}

object NOR extends ComboICGateLogic
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = ~shape<<1&0xE
    override def feedbackMask(shape:Int) = 1

    override def deadSides = 3

    override def calcOutput(gate:ComboGateICPart, input:Int) = if (input == 0) 1 else 0
}

object NOT extends ComboICGateLogic
{
    override def outputMask(shape:Int) = ~((shape&1)<<1|(shape&2)<<2)&0xB
    override def inputMask(shape:Int) = 4
    override def feedbackMask(shape:Int) = outputMask(shape)

    override def deadSides = 3

    override def calcOutput(gate:ComboGateICPart, input:Int) = if (input == 0) 0xB else 0
}

object AND extends ComboICGateLogic
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = ~shape<<1&0xE

    override def deadSides = 3

    override def calcOutput(gate:ComboGateICPart, input:Int) = if (input == inputMask(gate.shape)) 1 else 0
}

object NAND extends ComboICGateLogic
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = ~shape<<1&0xE

    override def deadSides = 3

    override def calcOutput(gate:ComboGateICPart, input:Int) = if (input == inputMask(gate.shape)) 0 else 1
}

object XOR extends ComboICGateLogic
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = 10

    override def calcOutput(gate:ComboGateICPart, input:Int) =
    {
        val side1 = (input&1<<1) != 0
        val side2 = (input&1<<3) != 0
        if (side1 != side2) 1 else 0
    }
}

object XNOR extends ComboICGateLogic
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = 10

    override def calcOutput(gate:ComboGateICPart, input:Int) =
    {
        val side1 = (input&1<<1) != 0
        val side2 = (input&1<<3) != 0
        if (side1 == side2) 1 else 0
    }
}

object Buffer extends ComboICGateLogic
{
    override def outputMask(shape:Int) = ~((shape&1)<<1|(shape&2)<<2)&0xB
    override def inputMask(shape:Int) = 4
    override def feedbackMask(shape:Int) = outputMask(shape)

    override def deadSides = 2
    override def maxDeadSides = 2

    override def calcOutput(gate:ComboGateICPart, input:Int) = if (input != 0) 0xB else 0
}

object Multiplexer extends ComboICGateLogic
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = 0xE

    override def calcOutput(gate:ComboGateICPart, input:Int) = if ((input&1<<2) != 0) (input>>3)&1 else (input>>1)&1
}

object Pulse extends ComboICGateLogic
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = 4

    override def calcOutput(gate:ComboGateICPart, input:Int) = 0

    override def onChange(gate:ComboGateICPart) =
    {
        val oldInput = gate.state&0xF
        val newInput = getInput(gate, 4)

        if (oldInput != newInput)
        {
            gate.setState(gate.state&0xF0|newInput)
            gate.onInputChange()
            if (newInput != 0 && (gate.state&0xF0) == 0)
            {
                gate.setState(gate.state&0xF|0x10)
                gate.scheduleTick(2)
                gate.onOutputChange(1)
            }
        }
    }
}

object Repeater extends ComboICGateLogic
{
    val delays = Array(2, 4, 6, 8, 16, 32, 64, 128, 256)

    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = 4

    override def getDelay(shape:Int) = delays(shape)

    override def cycleShape(shape:Int) = (shape+1)%delays.length

    override def calcOutput(gate:ComboGateICPart, input:Int) = if (input == 0) 0 else 1

    override def onChange(gate:ComboGateICPart){ if (gate.schedTime < 0) super.onChange(gate) }

    override def activate(gate:ComboGateICPart)
    {
        gate.configure()
    }

    @SideOnly(Side.CLIENT)
    override def getRolloverData(gate:ComboGateICPart, detailLevel:Int) =
    {
        val data = Seq.newBuilder[String]
        if (detailLevel > 1) data += "delay: "+delays(gate.shape)
        super.getRolloverData(gate, detailLevel)++data.result()
    }
}

object Randomizer extends ComboICGateLogic
{
    val rand = new Random

    override def outputMask(shape:Int) = ~((shape&1)<<1|(shape&2)>>1|(shape&4)<<1)&0xB
    override def inputMask(shape:Int) = 4
    override def feedbackMask(shape:Int) = outputMask(shape)

    override def deadSides = 3

    override def getDelay(shape:Int) = 2

    override def calcOutput(gate:ComboGateICPart, input:Int) =
    {
        if (input == 0) gate.state>>4 else
            outputMask(gate.shape)&TFaceOrient.shiftMask(rand.nextInt(8), 3)
    }

    override def onChange(gate:ComboGateICPart)
    {
        super.onChange(gate)
        if ((gate.state&4) != 0) gate.scheduleTick(2)
    }
}

object TransparentLatch extends ComboICGateLogic
{
    override def outputMask(shape:Int) = if (shape == 0) 3 else 9
    override def inputMask(shape:Int) = if (shape == 0) 0xC else 6

    override def cycleShape(shape:Int) = shape^1

    override def calcOutput(gate:ComboGateICPart, input:Int) =
    {
        if ((input&4) == 0) gate.state>>4
        else if ((input&0xA) == 0) 0 else 0xF
    }
}

object DecRandomizer extends ComboICGateLogic
{
    val rand = new Random

    override def cycleShape(shape:Int) = shape^1

    override def outputMask(shape:Int) = if (shape == 0) 11 else 9
    override def inputMask(shape:Int) = 4
    override def feedbackMask(shape:Int) = 2

    override def getDelay(shape:Int) = 2

    override def calcOutput(gate:ComboGateICPart, input:Int) =
    {
        if (input == 0) if ((gate.state>>4) == 0) 1 else gate.state>>4
        else Seq(1, 8, 2)(rand.nextInt((~gate.shape|2)&3))
    }

    override def onChange(gate:ComboGateICPart)
    {
        super.onChange(gate)
        if ((gate.state&4) != 0) gate.scheduleTick(2)
    }
}