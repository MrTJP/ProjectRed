/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.integration

import codechicken.multipart.util.PartRayTraceResult
import mrtjp.projectred.api.IScrewdriver
import mrtjp.projectred.core.TFaceOrient
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.ItemStack
import net.minecraft.world.LightType

import java.util.Random

class ComboGatePart(gateType:GateType) extends RedstoneGatePart(gateType)
{
    override def getLogic[T] = ComboGateLogic.instances(gateType.ordinal()).asInstanceOf[T]
    def getLogicCombo = getLogic[ComboGateLogic]

//    override def getType = GateDefinition.typeSimpleGate
}

object ComboGateLogic
{
    val advanceDead = Seq(1, 2, 4, 0, 5, 6, 3)

    val instances = new Array[ComboGateLogic](GateType.values().length)
    initialize()

    def initialize()
    {
        instances(GateType.OR.ordinal) = OR
        instances(GateType.NOR.ordinal) = NOR
        instances(GateType.NOT.ordinal) = NOT
        instances(GateType.AND.ordinal) = AND
        instances(GateType.NAND.ordinal) = NAND
        instances(GateType.XOR.ordinal) = XOR
        instances(GateType.XNOR.ordinal) = XNOR
        instances(GateType.BUFFER.ordinal) = Buffer
        instances(GateType.MULTIPLEXER.ordinal) = Multiplexer
        instances(GateType.PULSE.ordinal) = Pulse
        instances(GateType.REPEATER.ordinal) = Repeater
        instances(GateType.RANDOMIZER.ordinal) = Randomizer

        instances(GateType.TRANSPARENT_LATCH.ordinal) = TransparentLatch
        instances(GateType.LIGHT_SENSOR.ordinal) = LightSensor
        instances(GateType.RAIN_SENSOR.ordinal) = RainSensor

        instances(GateType.DEC_RANDOMIZER.ordinal) = DecodingRand
    }
}

trait TSimpleRSGateLogic[T <: RedstoneGatePart] extends RedstoneGateLogic[T]
{
    def getDelay(shape:Int) = 2

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
}

abstract class ComboGateLogic extends RedstoneGateLogic[ComboGatePart] with TSimpleRSGateLogic[ComboGatePart]
{
    override def cycleShape(gate:ComboGatePart) =
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

    def cycleShape(shape:Int):Int =
    {
        if (deadSides == 0) return shape

        var shape1 = shape
        import java.lang.Integer.{bitCount, numberOfLeadingZeros => lead}
        do shape1 = ComboGateLogic.advanceDead(shape1)
        while (bitCount(shape1) > maxDeadSides || 32-lead(shape1) > deadSides)
        shape1
    }

    def deadSides = 0
    def maxDeadSides = deadSides-1
}

object OR extends ComboGateLogic
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = ~shape<<1&0xE

    override def deadSides = 3

    override def calcOutput(gate:ComboGatePart, input:Int) = if (input != 0) 1 else 0
}

object NOR extends ComboGateLogic
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = ~shape<<1&0xE
    override def feedbackMask(shape:Int) = 1

    override def deadSides = 3

    override def calcOutput(gate:ComboGatePart, input:Int) = if (input == 0) 1 else 0
}

object NOT extends ComboGateLogic
{
    override def outputMask(shape:Int) = ~((shape&1)<<1|(shape&2)>>1|(shape&4)<<1)&0xB
    override def inputMask(shape:Int) = 4
    override def feedbackMask(shape:Int) = outputMask(shape)

    override def deadSides = 3

    override def calcOutput(gate:ComboGatePart, input:Int) = if (input == 0) 0xB else 0
}

object AND extends ComboGateLogic
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = ~shape<<1&0xE

    override def deadSides = 3

    override def calcOutput(gate:ComboGatePart, input:Int) = if (input == inputMask(gate.shape)) 1 else 0
}

object NAND extends ComboGateLogic
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = ~shape<<1&0xE

    override def deadSides = 3

    override def calcOutput(gate:ComboGatePart, input:Int) = if (input == inputMask(gate.shape)) 0 else 1
}

object XOR extends ComboGateLogic
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = 10

    override def calcOutput(gate:ComboGatePart, input:Int) =
    {
        val side1 = (input&1<<1) != 0
        val side2 = (input&1<<3) != 0
        if (side1 != side2) 1 else 0
    }
}

object XNOR extends ComboGateLogic
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = 10

    override def calcOutput(gate:ComboGatePart, input:Int) =
    {
        val side1 = (input&1<<1) != 0
        val side2 = (input&1<<3) != 0
        if (side1 == side2) 1 else 0
    }
}

object Buffer extends ComboGateLogic
{
    override def outputMask(shape:Int) = ~((shape&1)<<1|(shape&2)<<2)&0xB
    override def inputMask(shape:Int) = 4
    override def feedbackMask(shape:Int) = outputMask(shape)

    override def deadSides = 2
    override def maxDeadSides = 2

    override def calcOutput(gate:ComboGatePart, input:Int) = if (input != 0) 0xB else 0
}

object Multiplexer extends ComboGateLogic
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = 0xE

    override def calcOutput(gate:ComboGatePart, input:Int) = if ((input&1<<2) != 0) (input>>3)&1 else (input>>1)&1
}

object Pulse extends ComboGateLogic
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = 4

    override def calcOutput(gate:ComboGatePart, input:Int) = 0

    override def onChange(gate:ComboGatePart) =
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

object Repeater extends ComboGateLogic
{
    val delays = Array(2, 4, 6, 8, 16, 32, 64, 128, 256)

    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = 4

    override def getDelay(shape:Int) = delays(shape)

    override def cycleShape(shape:Int) = (shape+1)%delays.length

    override def calcOutput(gate:ComboGatePart, input:Int) = if (input == 0) 0 else 1

    override def onChange(gate:ComboGatePart){ if (gate.schedTime < 0) super.onChange(gate) }

    override def activate(gate:ComboGatePart, player:PlayerEntity, held:ItemStack, hit:PartRayTraceResult)=
    {
        if (held.isEmpty || !held.getItem.isInstanceOf[IScrewdriver])
        {
            if (!gate.world.isRemote) gate.configure()
            true
        }
        else false
    }
}

object Randomizer extends ComboGateLogic
{
    val rand = new Random

    override def outputMask(shape:Int) = ~((shape&1)<<1|(shape&2)>>1|(shape&4)<<1)&0xB
    override def inputMask(shape:Int) = 4
    override def feedbackMask(shape:Int) = outputMask(shape)

    override def deadSides = 3

    override def calcOutput(gate:ComboGatePart, input:Int) =
    {
        if (input == 0) gate.state>>4 else
            outputMask(gate.shape)&TFaceOrient.shiftMask(rand.nextInt(8), 3)
    }

    override def onChange(gate:ComboGatePart)
    {
        super.onChange(gate)
        if ((gate.state&4) != 0) gate.scheduleTick(2)
    }
}

object TransparentLatch extends ComboGateLogic
{
    override def outputMask(shape:Int) = if (shape == 0) 3 else 9
    override def inputMask(shape:Int) = if (shape == 0) 0xC else 6

    override def cycleShape(shape:Int) = shape^1

    override def calcOutput(gate:ComboGatePart, input:Int) =
    {
        if ((input&4) == 0) gate.state>>4
        else if ((input&0xA) == 0) 0 else 0xF
    }
}

object LightSensor extends ComboGateLogic
{
    override def outputMask(shape:Int) = 4
    override def inputMask(shape:Int) = 0
    override def feedbackMask(shape:Int) = 4

    override def cycleShape(shape:Int) = (shape+1)%3

    override def getOutput(gate:ComboGatePart, r:Int) = if (r == 2) gate.state>>4 else 0

    override def setup(gate:ComboGatePart){ onTick(gate) }

    override def onTick(gate:ComboGatePart)
    {
        if (gate.world.isRemote) return

        def sky = gate.world.getLightFor(LightType.SKY, gate.pos)-gate.world.getSkylightSubtracted
        def block = gate.world.getLightFor(LightType.BLOCK, gate.pos)

        val shape = gate.shape
        val newOutput = shape match
        {
            case 1 => sky
            case 2 => block
            case _ => Math.max(sky, block)
        }

        if (newOutput != (gate.state>>4))
        {
            gate.setState(newOutput<<4|gate.state&0xF)
            gate.onOutputChange(4)
        }
    }

    override def onChange(gate:ComboGatePart)
    {
        val oldInput = gate.state&0xF
        val newInput = getInput(gate, 4)
        if (oldInput != newInput)
        {
            gate.setState(gate.state&0xF0|newInput)
            gate.onInputChange()
        }
    }

    override def lightLevel = 0
}

object RainSensor extends ComboGateLogic
{
    override def outputMask(shape:Int) = 4
    override def inputMask(shape:Int) = 0
    override def feedbackMask(shape:Int) = 4

    override def onTick(gate:ComboGatePart)
    {
        if (gate.world.isRemote) return

        val newOutput = if (gate.world.isRaining && gate.world.canBlockSeeSky(gate.pos)) 4 else 0
        val oldOutput = gate.state>>4
        if (newOutput != oldOutput)
        {
            gate.setState(newOutput<<4|gate.state&0xF)
            gate.onOutputChange(4)
        }
    }

    override def lightLevel = 0
}

object DecodingRand extends ComboGateLogic
{
    val rand = new Random

    override def cycleShape(shape:Int) = shape^1

    override def outputMask(shape:Int) = if (shape == 0) 11 else 9
    override def inputMask(shape:Int) = 4
    override def feedbackMask(shape:Int) = 2

    override def calcOutput(gate:ComboGatePart, input:Int) =
    {
        if (input == 0) if ((gate.state>>4) == 0) 1 else gate.state>>4
        else Seq(1, 8, 2)(rand.nextInt((~gate.shape|2)&3))
    }

    override def onChange(gate:ComboGatePart)
    {
        super.onChange(gate)
        if ((gate.state&4) != 0) gate.scheduleTick(2)
    }
}
