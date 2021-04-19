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

abstract class ComboGatePart(gateType:GateType) extends RedstoneGatePart(gateType) with TSimpleRSGatePart with TDeadSideCyclerGate

trait TDeadSideCyclerGate extends RedstoneGatePart
{
    override def gateLogicCycleShape():Boolean = {
        val oldShape = shape
        val newShape = cycleShape(oldShape)
        if (newShape != oldShape) {
            setShape(newShape)
            true
        }
        else false
    }

    def cycleShape(shape:Int):Int = {
        if (deadSides != 0) {
            var shape1 = shape
            do shape1 = TDeadSideCyclerGate.advanceDead(shape1)
            while (Integer.bitCount(shape1) > maxDeadSides || 32-Integer.numberOfLeadingZeros(shape1) > deadSides)
            shape1
        } else
            shape
    }

    def deadSides:Int = 0
    def maxDeadSides:Int = deadSides-1
}

object TDeadSideCyclerGate
{
    val advanceDead = Seq(1, 2, 4, 0, 5, 6, 3)
}

trait TSimpleRSGatePart extends RedstoneGatePart
{
    def getDelay(shape:Int) = 2

    def feedbackMask(shape:Int) = 0

    def calcOutput(input:Int) = 0

    override def gateLogicOnChange():Unit = {
        val iMask = inputMask(shape)
        val oMask = outputMask(shape)
        val fMask = feedbackMask(shape)
        val oldInput = state&0xF
        val newInput = getInput(iMask|fMask)
        if (oldInput != newInput) {
            setState(state&0xF0|newInput)
            onInputChange()
        }

        val newOutput = calcOutput(state&iMask)&oMask
        if (newOutput != (state>>4)) scheduleTick(getDelay(shape))
    }

    override def gateLogicOnScheduledTick():Unit = {
        val iMask = inputMask(shape)
        val oMask = outputMask(shape)
        val oldOutput = state>>4
        val newOutput = calcOutput(state&iMask)&oMask
        if (oldOutput != newOutput) {
            setState(state&0xF|newOutput<<4)
            onOutputChange(oMask)
        }
        gateLogicOnChange()
    }

    override def gateLogicSetup():Unit = {
        val iMask = inputMask(shape)
        val oMask = outputMask(shape)
        val output = calcOutput(getInput(iMask))&oMask
        if (output != 0) {
            setState(output<<4)
            onOutputChange(output) //use output for change mask because nothing is going low
        }
    }
}

class OR extends ComboGatePart(GateType.OR)
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int):Int = ~shape<<1&0xE

    override def deadSides = 3

    override def calcOutput(input:Int):Int = if (input != 0) 1 else 0
}

class NOR extends ComboGatePart(GateType.NOR)
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int):Int = ~shape<<1&0xE
    override def feedbackMask(shape:Int) = 1

    override def deadSides = 3

    override def calcOutput(input:Int):Int = if (input == 0) 1 else 0
}

class NOT extends ComboGatePart(GateType.NOT)
{
    override def outputMask(shape:Int):Int = ~((shape&1)<<1|(shape&2)>>1|(shape&4)<<1)&0xB
    override def inputMask(shape:Int) = 4
    override def feedbackMask(shape:Int):Int = outputMask(shape)

    override def deadSides = 3

    override def calcOutput(input:Int):Int = if (input == 0) 0xB else 0
}

class AND extends ComboGatePart(GateType.AND)
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int):Int = ~shape<<1&0xE

    override def deadSides = 3

    override def calcOutput(input:Int):Int = if (input == inputMask(shape)) 1 else 0
}

class NAND extends ComboGatePart(GateType.NAND)
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int):Int = ~shape<<1&0xE

    override def deadSides = 3

    override def calcOutput(input:Int):Int = if (input == inputMask(shape)) 0 else 1
}

class XOR extends ComboGatePart(GateType.XOR)
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = 10

    override def calcOutput(input:Int):Int = {
        val side1 = (input&1<<1) != 0
        val side2 = (input&1<<3) != 0
        if (side1 != side2) 1 else 0
    }
}

class XNOR extends ComboGatePart(GateType.XNOR)
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = 10

    override def calcOutput(input:Int):Int =  {
        val side1 = (input&1<<1) != 0
        val side2 = (input&1<<3) != 0
        if (side1 == side2) 1 else 0
    }
}

class Buffer extends ComboGatePart(GateType.BUFFER)
{
    override def outputMask(shape:Int):Int = ~((shape&1)<<1|(shape&2)<<2)&0xB
    override def inputMask(shape:Int) = 4
    override def feedbackMask(shape:Int):Int = outputMask(shape)

    override def deadSides = 2
    override def maxDeadSides = 2

    override def calcOutput(input:Int):Int = if (input != 0) 0xB else 0
}

class Multiplexer extends ComboGatePart(GateType.MULTIPLEXER)
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = 0xE

    override def calcOutput(input:Int):Int = if ((input&1<<2) != 0) (input>>3)&1 else (input>>1)&1
}

class Pulse extends ComboGatePart(GateType.PULSE)
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = 4

    override def calcOutput(input:Int):Int = 0

    override def gateLogicOnChange():Unit = {
        val oldInput = state&0xF
        val newInput = getInput(4)

        if (oldInput != newInput) {
            setState(state&0xF0|newInput)
            onInputChange()
            if (newInput != 0 && (state&0xF0) == 0) {
                setState(state&0xF|0x10)
                scheduleTick(2)
                onOutputChange(1)
            }
        }
    }
}

class Repeater extends ComboGatePart(GateType.REPEATER)
{
    val delays = Array(2, 4, 6, 8, 16, 32, 64, 128, 256)

    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = 4

    override def getDelay(shape:Int):Int = delays(shape)

    override def cycleShape(shape:Int):Int = (shape+1)%delays.length

    override def calcOutput(input:Int):Int = if (input == 0) 0 else 1

    override def gateLogicOnChange(){ if (schedTime < 0) super.gateLogicOnChange() }

    // Allow configuring without screwdriver
    override def gateLogicActivate(player:PlayerEntity, held:ItemStack, hit:PartRayTraceResult):Boolean = {
        if (held.isEmpty || !held.getItem.isInstanceOf[IScrewdriver]) {
            if (!world.isRemote) configure()
            true
        } else
            false
    }
}

class Randomizer extends ComboGatePart(GateType.RANDOMIZER)
{
    val rand = new Random

    override def outputMask(shape:Int):Int = ~((shape&1)<<1|(shape&2)>>1|(shape&4)<<1)&0xB
    override def inputMask(shape:Int) = 4
    override def feedbackMask(shape:Int):Int = outputMask(shape)

    override def deadSides = 3

    override def calcOutput(input:Int):Int = {
        if (input == 0) state>>4 else
            outputMask(shape)&TFaceOrient.shiftMask(rand.nextInt(8), 3)
    }

    override def gateLogicOnChange() {
        super.gateLogicOnChange()
        if ((state&4) != 0) scheduleTick(2)
    }
}

class TransparentLatch extends ComboGatePart(GateType.TRANSPARENT_LATCH)
{
    override def outputMask(shape:Int):Int = if (shape == 0) 3 else 9
    override def inputMask(shape:Int):Int = if (shape == 0) 0xC else 6

    override def cycleShape(shape:Int):Int = shape^1

    override def calcOutput(input:Int):Int = {
        if ((input&4) == 0) state>>4
        else if ((input&0xA) == 0) 0 else 0xF
    }
}

class LightSensor extends ComboGatePart(GateType.LIGHT_SENSOR)
{
    override def outputMask(shape:Int) = 4
    override def inputMask(shape:Int) = 0
    override def feedbackMask(shape:Int) = 4

    override def cycleShape(shape:Int):Int = (shape+1)%3

    override def getOutput(r:Int):Int = if (r == 2) state>>4 else 0

    override def gateLogicSetup(){ gateLogicOnTick() }

    override def gateLogicOnTick():Unit = {
        if (world.isRemote) return

        def sky:Int = world.getLightFor(LightType.SKY, pos)-world.getSkylightSubtracted
        def block:Int = world.getLightFor(LightType.BLOCK, pos)

        val newOutput = shape match {
            case 1 => sky
            case 2 => block
            case _ => Math.max(sky, block)
        }

        if (newOutput != (state>>4)) {
            setState(newOutput<<4|state&0xF)
            onOutputChange(4)
        }
    }

    override def gateLogicOnChange():Unit = {
        val oldInput = state&0xF
        val newInput = getInput(4)
        if (oldInput != newInput) {
            setState(state&0xF0|newInput)
            onInputChange()
        }
    }

    override def getLightValue:Int = 0
}

class RainSensor extends ComboGatePart(GateType.RAIN_SENSOR)
{
    override def outputMask(shape:Int) = 4
    override def inputMask(shape:Int) = 0
    override def feedbackMask(shape:Int) = 4

    override def gateLogicOnTick():Unit = {
        if (!world.isRemote) {
            val newOutput = if (world.isRaining && world.canBlockSeeSky(pos)) 4 else 0
            val oldOutput = state>>4
            if (newOutput != oldOutput) {
                setState(newOutput<<4|state&0xF)
                onOutputChange(4)
            }
        }
    }

    override def getLightValue:Int = 0
}

class DecodingRand extends ComboGatePart(GateType.DEC_RANDOMIZER)
{
    val rand = new Random

    override def cycleShape(shape:Int):Int = shape^1

    override def outputMask(shape:Int):Int = if (shape == 0) 11 else 9
    override def inputMask(shape:Int) = 4
    override def feedbackMask(shape:Int) = 2

    override def calcOutput(input:Int):Int = {
        if (input == 0) if ((state>>4) == 0) 1 else state>>4
        else Seq(1, 8, 2)(rand.nextInt((~shape|2)&3))
    }

    override def gateLogicOnChange():Unit ={
        super.gateLogicOnChange()
        if ((state&4) != 0) scheduleTick(2)
    }
}
