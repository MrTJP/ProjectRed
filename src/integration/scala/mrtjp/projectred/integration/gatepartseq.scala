/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.integration

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.vec.Rotation
import codechicken.multipart.api.part.INeighborTileChangePart
import codechicken.multipart.util.PartRayTraceResult
import com.google.common.base.Predicate
import mrtjp.projectred.api.IScrewdriver
import mrtjp.projectred.core.{Configurator, IRedwireEmitter}
import mrtjp.projectred.core.TFaceOrient._
import net.minecraft.block.material.Material
import net.minecraft.entity.Entity
import net.minecraft.entity.item.ItemFrameEntity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.ItemStack
import net.minecraft.nbt.CompoundNBT
import net.minecraft.util.Direction
import net.minecraft.util.math.{AxisAlignedBB, BlockPos}
import net.minecraft.world.World

import java.util

trait TExtraStateGatePart extends RedstoneGatePart
{
    private var lState2:Byte = 0

    def state2 = lState2&0xFF
    def setState2(state:Int){ lState2 = state.toByte }

    def clientState2 = false

    abstract override def save(tag:CompoundNBT):Unit = {
        super.save(tag)
        tag.putByte("state2", lState2)
    }

    abstract override def load(tag:CompoundNBT):Unit = {
        super.load(tag)
        lState2 = tag.getByte("state2")
    }

    abstract override def writeDesc(packet:MCDataOutput):Unit = {
        super.writeDesc(packet)
        if (clientState2) packet.writeByte(lState2)
    }

    abstract override def readDesc(packet:MCDataInput):Unit = {
        super.readDesc(packet)
        if (clientState2) lState2 = packet.readByte()
    }

    abstract override def read(packet:MCDataInput, key:Int):Unit = key match {
        case 11 => lState2 = packet.readByte()
        case _ => super.read(packet, key)
    }

    def sendState2Update():Unit = {
        sendUpdate(11, _.writeByte(lState2))
    }
}

class SRLatch extends RedstoneGatePart(GateType.SR_LATCH) with TExtraStateGatePart
{
    override def outputMask(shape:Int) = if ((shape>>1) == 0) 0xF else 5
    override def inputMask(shape:Int) = 0xA

    override def gateLogicCycleShape():Boolean = {
        setShape((shape+1)%4)
        setState2(flipMaskZ(state2))
        setState(flipMaskZ(state))
        onOutputChange(0xF)
        scheduleTick(2)
        true
    }

    override def gateLogicSetup():Unit = {
        setState2(2)
        setState(0x30)
    }

    override def gateLogicOnChange():Unit = {
        val stateInput = state2

        val oldInput = state&0xF
        val newInput = getInput(0xA)
        val oldOutput = state>>4

        if (newInput != oldInput) {
            if (stateInput != 0xA && newInput != 0 && newInput != stateInput) { //state needs changing
                setState(newInput)
                setState2(newInput)
                onOutputChange(oldOutput) //always going low
                scheduleTick(2)
            } else {
                setState(oldOutput<<4|newInput)
                onInputChange()
            }
        }
    }

    override def gateLogicOnScheduledTick():Unit = {
        val oldOutput = state>>4
        val newOutput = calcOutput()

        if (oldOutput != newOutput) {
            setState(state&0xF|newOutput<<4)
            onOutputChange(outputMask(shape))
        }
        gateLogicOnChange()
    }

    def calcOutput():Int = {
        var input = state&0xF
        var stateInput = state2

        if ((shape&1) != 0) //reverse
        {
            input = flipMaskZ(input)
            stateInput = flipMaskZ(stateInput)
        }

        if (stateInput == 0xA) //disabled
        {
            if (input == 0xA)
            {
                scheduleTick(2)
                return 0
            }

            stateInput =
                if (input == 0) if (world.random.nextBoolean()) 2 else 8
                else input

            setState2(if ((shape&1) != 0) flipMaskZ(stateInput) else stateInput)
        }

        var output = shiftMask(stateInput, 1)
        if ((shape&2) == 0) output |= stateInput
        if ((shape&1) != 0) output = flipMaskZ(output) //reverse
        output
    }
}

class ToggleLatch extends RedstoneGatePart(GateType.TOGGLE_LATCH) with TExtraStateGatePart
{
    override def outputMask(shape:Int) = 5
    override def inputMask(shape:Int) = 0xA

    override def clientState2 = true


    override def gateLogicSetup():Unit = {
        setState(0x10)
        sendStateUpdate()
    }

    override def gateLogicOnChange():Unit = {
        val oldInput = state&0xF
        val newInput = getInput(0xA)
        val high = newInput& ~oldInput

        if (high == 2 || high == 8) toggle()

        if (oldInput != newInput) {
            setState(state&0xF0|newInput)
            onInputChange()
        }
    }

    override def gateLogicOnScheduledTick():Unit = {
        val oldOutput = state>>4
        val newOutput = if (state2 == 0) 1 else 4
        if (oldOutput != newOutput) {
            setState(newOutput<<4|state&0xF)
            onOutputChange(5)
        }
        gateLogicOnChange()
    }

    // Allow toggling with empty hand
    override def gateLogicActivate(player:PlayerEntity, held:ItemStack, hit:PartRayTraceResult):Boolean = {
        if (held.isEmpty || !held.getItem.isInstanceOf[IScrewdriver]) {
            if (!world.isClientSide) toggle()
            true
        } else false
    }

    def toggle():Unit =  {
        setState2(state2^1)
        scheduleTick(2)
        tickSound()
    }
}

trait ITimerGuiLogic extends GatePart
{
    def getTimerMax:Int
    def setTimerMax(t:Int)
}

trait ICounterGuiLogic extends GatePart
{
    def getCounterMax:Int
    def setCounterMax(i:Int)

    def getCounterIncr:Int
    def setCounterIncr(i:Int)

    def getCounterDecr:Int
    def setCounterDecr(i:Int)

    def getCounterValue:Int
    def setCounterValue(i:Int)
}

trait TTimerGatePart extends RedstoneGatePart with ITimerGuiLogic
{
    var pointer_max = 38
    var pointer_start = -1L

    abstract override def save(tag:CompoundNBT):Unit = {
        super.save(tag)
        tag.putInt("pmax", pointer_max)
        tag.putLong("pelapsed", if (pointer_start < 0) pointer_start else world.getGameTime-pointer_start)
    }

    abstract override def load(tag:CompoundNBT):Unit = {
        super.load(tag)
        pointer_max = tag.getInt("pmax")
        pointer_start = tag.getLong("pelapsed")
    }

    abstract override def writeDesc(packet:MCDataOutput):Unit = {
        super.writeDesc(packet)
        packet.writeInt(pointer_max)
        packet.writeLong(pointer_start)
    }

    abstract override def readDesc(packet:MCDataInput):Unit = {
        super.readDesc(packet)
        pointer_max = packet.readInt()
        pointer_start = packet.readLong()
    }

    abstract override def read(packet:MCDataInput, key:Int):Unit = key match {
        case 12 => pointer_max = packet.readInt()
        case 13 =>
            pointer_start = packet.readInt()
            if (pointer_start >= 0) pointer_start = world.getGameTime-pointer_start
        case _ => super.read(packet, key)
    }


    override def gateLogicOnWorldLoad():Unit = {
        if (pointer_start >= 0) pointer_start = world.getGameTime-pointer_start
    }

    def pointerValue:Int =
        if (pointer_start < 0) 0
        else (world.getGameTime-pointer_start).toInt

    def sendPointerMaxUpdate():Unit = { sendUpdate(12, _.writeInt(pointer_max)) }
    def sendPointerUpdate():Unit = { sendUpdate(13, _.writeInt(if (pointer_start < 0) -1 else pointerValue)) }

    override def getTimerMax:Int = pointer_max+2
    override def setTimerMax(time:Int):Unit = {
        var t = time
        val minTime = math.max(4, Configurator.minTimerTicks)
        if (t < minTime) t = minTime
        if (t != pointer_max) {
            pointer_max = t-2
            sendPointerMaxUpdate()
        }
    }

    override def gateLogicOnTick():Unit = {
        if (pointer_start >= 0)
            if (world.getGameTime >= pointer_start+pointer_max) pointerTick()
            else if (pointer_start > world.getGameTime)
                pointer_start = world.getGameTime
    }

    def pointerTick():Unit

    def resetPointer():Unit = {
        if (pointer_start >= 0) {
            pointer_start = -1
            tile.setChanged()
            if (!world.isClientSide) sendPointerUpdate()
        }
    }

    def startPointer():Unit = {
        if (pointer_start < 0) {
            pointer_start = world.getGameTime
            tile.setChanged()
            if (!world.isClientSide) sendPointerUpdate()
        }
    }

    def interpPointer(f:Float):Float = if (pointer_start < 0) 0f else (pointerValue+f)/pointer_max

    override def gateLogicActivate(player:PlayerEntity, held:ItemStack, hit:PartRayTraceResult):Boolean = {
        if (held.isEmpty || !held.getItem.isInstanceOf[IScrewdriver]) {
            if (!world.isClientSide) GuiTimer.open(player, this)
            true
        } else false
    }
}

class Timer extends RedstoneGatePart(GateType.TIMER) with TTimerGatePart
{
    override def outputMask(shape:Int) = 0xB
    override def inputMask(shape:Int) = 0xE

    override def gateLogicSetup():Unit = { startPointer() }

    override def gateLogicOnScheduledTick():Unit = {
        setState(state&0xF)
        onOutputChange(0xB)
        gateLogicOnChange()
    }

    override def gateLogicOnChange():Unit = {
        val oldInput = state&0xF
        val newInput = getInput(0xE)

        if (newInput != oldInput) {
            setState(state&0xF0|newInput)
            onInputChange()
        }

        if (schedTime < 0) {
            if (newInput > 0) resetPointer() else startPointer()
        }
    }

    override def pointerTick():Unit = {
        resetPointer()
        if (!world.isClientSide) {
            scheduleTick(2)
            setState(0xB0|state&0xF)
            onOutputChange(0xB)
            tickSound()
        }
    }
}

class Sequencer extends RedstoneGatePart(GateType.SEQUENCER) with ITimerGuiLogic
{
    var pointer_max = 40

    override def outputMask(shape:Int) = 0xF

    override def gateLogicOnChange():Unit = {}
    override def gateLogicOnScheduledTick():Unit = {}

    override def getTimerMax:Int = pointer_max
    override def setTimerMax(time:Int) {
        var t = time
        val minTime = math.max(4, Configurator.minTimerTicks)
        if (t < minTime) t = minTime
        if (t != pointer_max) {
            pointer_max = t
            sendPointerMaxUpdate()
        }
    }

    override def save(tag:CompoundNBT):Unit = {
        super.save(tag)
        tag.putInt("pmax", pointer_max)
    }
    override def load(tag:CompoundNBT):Unit = {
        super.load(tag)
        pointer_max = tag.getInt("pmax")
    }

    override def writeDesc(packet:MCDataOutput):Unit = {
        super.writeDesc(packet)
        packet.writeInt(pointer_max)
    }

    override def readDesc(packet:MCDataInput):Unit = {
        super.readDesc(packet)
        pointer_max = packet.readInt()
    }

    override def read(packet:MCDataInput, key:Int):Unit = key match {
        case 12 => pointer_max = packet.readInt()
        case _ => super.read(packet, key)
    }

    def sendPointerMaxUpdate():Unit = { sendUpdate(12, _.writeInt(pointer_max)) }

    override def gateLogicOnTick():Unit = {
        if (!world.isClientSide) {
            val oldOut = state>>4
            var out = 1<<world.getDayTime%(pointer_max*4)/pointer_max
            if (shape == 1) out = flipMaskZ(out)
            if (oldOut != out) {
                setState(out<<4)
                onOutputChange(0xF)
                tickSound()
            }
        }
    }

    override def gateLogicCycleShape():Boolean = {
        setShape(shape^1)
        true
    }

    override def gateLogicActivate(player:PlayerEntity, held:ItemStack, hit:PartRayTraceResult):Boolean = {
        if (held.isEmpty || !held.getItem.isInstanceOf[IScrewdriver]) {
            if (!world.isClientSide) GuiTimer.open(player, this)
            true
        } else false
    }
}

class Counter extends RedstoneGatePart(GateType.COUNTER) with ICounterGuiLogic
{
    var value = 0
    var max = 10
    var incr = 1
    var decr = 1

    override def outputMask(shape:Int) = 5
    override def inputMask(shape:Int) = 10

    override def save(tag:CompoundNBT):Unit = {
        super.save(tag)
        tag.putInt("val", value)
        tag.putInt("max", max)
        tag.putInt("inc", incr)
        tag.putInt("dec", decr)
    }

    override def load(tag:CompoundNBT):Unit = {
        super.load(tag)
        value = tag.getInt("val")
        max = tag.getInt("max")
        incr = tag.getInt("inc")
        decr = tag.getInt("dec")
    }

    override def writeDesc(packet:MCDataOutput):Unit = {
        super.writeDesc(packet)
        packet.writeInt(value).writeInt(max).writeInt(incr).writeInt(decr)
    }

    override def readDesc(packet:MCDataInput):Unit = {
        super.readDesc(packet)
        value = packet.readInt()
        max = packet.readInt()
        incr = packet.readInt()
        decr = packet.readInt()
    }

    override def read(packet:MCDataInput, key:Int):Unit = key match {
        case 11 => value = packet.readInt()
        case 12 => max = packet.readInt()
        case 13 => incr = packet.readInt()
        case 14 => decr = packet.readInt()
        case _ => super.read(packet, key)
    }

    def sendValueUpdate():Unit = { sendUpdate(11, _.writeInt(value)) }
    def sendMaxUpdate():Unit = { sendUpdate(12, _.writeInt(max)) }
    def sendIncrUpdate():Unit = { sendUpdate(13, _.writeInt(incr)) }
    def sendDecrUpdate():Unit = { sendUpdate(14, _.writeInt(decr)) }

    override def getCounterValue:Int = value
    override def getCounterMax:Int = max
    override def getCounterIncr:Int = incr
    override def getCounterDecr:Int = decr

    override def setCounterValue(i:Int):Unit = {
        val oldVal = value
        value = Math.min(max, Math.max(0, i))
        if (value != oldVal) {
            tickSound()
            sendValueUpdate()
        }
    }

    override def setCounterMax(i:Int):Unit = {
        val oldMax = max
        max = Math.min(32767, Math.max(1, i))
        if (max != oldMax) {
            tickSound()
            sendMaxUpdate()
            val oldVal = value
            value = Math.min(value, Math.max(0, i))
            if (value != oldVal) {
                sendValueUpdate()
                scheduleTick(2)
            }
        }
    }

    override def setCounterIncr(i:Int):Unit = {
        val oldIncr = incr
        incr = Math.min(max, Math.max(1, i))
        if (incr != oldIncr) {
            tickSound()
            sendIncrUpdate()
        }
    }

    override def setCounterDecr(i:Int):Unit = {
        val oldDecr = decr
        decr = Math.min(max, Math.max(1, i))
        if (decr != oldDecr) {
            tickSound()
            sendDecrUpdate()
        }
    }

    def gateLogicOnChange():Unit = {
        val oldInput = state&0xF
        var newInput = getInput(0xA)
        if (shape == 1) newInput = flipMaskZ(newInput)
        val high = newInput& ~oldInput

        if ((high&2) != 0) setCounterValue(value+incr)
        if ((high&8) != 0) setCounterValue(value-decr)
        if (oldInput != newInput) {
            setState(state&0xF0|newInput)
            onInputChange()
            scheduleTick(2)
        }
    }

    override def gateLogicCycleShape():Boolean = {
        setShape(if (shape == 1) 0 else 1)
        true
    }

    def gateLogicOnScheduledTick():Unit = {
        val oldOutput = state
        var newOutput = 0
        if (value == max) newOutput = 1
        else if (value == 0) newOutput = 4
        if (newOutput != oldOutput) setState(state&0xF|newOutput<<4)
        if (newOutput != oldOutput) onOutputChange(5)
    }

    override def gateLogicActivate(player:PlayerEntity, held:ItemStack, hit:PartRayTraceResult):Boolean = {
        if (held.isEmpty || !held.getItem.isInstanceOf[IScrewdriver]) {
            if (!world.isClientSide) GuiCounter.open(player, this)
            true
        } else false
    }
}

class StateCell extends RedstoneGatePart(GateType.STATE_CELL) with TTimerGatePart with TExtraStateGatePart
{
    override def outputMask(shape:Int):Int = {
        var output = 9
        if (shape == 1) output = flipMaskZ(output)
        output
    }

    override def inputMask(shape:Int):Int = {
        var input = 6
        if (shape == 1) input = flipMaskZ(input)
        input
    }

    override def gateLogicCycleShape():Boolean = {
        setShape((shape+1)%2)
        true
    }

    override def gateLogicOnChange():Unit = {
        val oldInput = state&0xF
        var newInput = getInput(0xE)
        if (oldInput != newInput) {
            setState(state&0xF0|newInput)
            onInputChange()

            if (shape == 1) newInput = flipMaskZ(newInput)
            if ((newInput&4) != 0 && state2 == 0) {
                setState2(1)
                sendState2Update()
                scheduleTick(2)
            }

            if (state2 != 0) if ((newInput&6) != 0) resetPointer()
            else startPointer()
        }
    }

    override def pointerTick()
    {
        resetPointer()
        if (!world.isClientSide) {
            setState2(0)
            sendState2Update()
            setState(0x10|state&0xF)
            onOutputChange(outputMask(shape))
            scheduleTick(2)
            tickSound()
        }
    }

    override def gateLogicOnScheduledTick():Unit = {
        var output = 0
        if (state2 != 0) output = 8
        if (shape == 1) output = flipMaskZ(output)

        setState(output<<4|state&0xF)
        onOutputChange(outputMask(shape))
    }
}

class Synchronizer extends RedstoneGatePart(GateType.SYNCHRONIZER) with TExtraStateGatePart
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = 14

    override def gateLogicOnChange():Unit = {
        val oldInput = state&0xF
        val newInput = getInput(14)
        val high = newInput& ~oldInput
        if (oldInput != newInput) {
            val oldValue = state2

            setState(state&0xF0|newInput)
            onInputChange()
            if ((newInput&4) != 0)
                setState2(0)
            else {
                if ((high&2) != 0) setState2(state2|1) //right
                if ((high&8) != 0) setState2(state2|2) //left
            }
            if (right && left) scheduleTick(2)

            if (state2 != oldValue) sendState2Update()
        }
    }

    override def gateLogicOnScheduledTick():Unit = {
        val oldValue = state2
        if (!pulsing && right && left) {
            setState(state|1<<4)
            onOutputChange(1)
            setState2(state2|4) //pulsing
            scheduleTick(2)
        } else if (pulsing) {
            setState(state& ~0x10)
            onOutputChange(1)
            setState2(0) //off
        }
        if (state2 != oldValue) sendState2Update()
    }

    def right:Boolean = (state2&1) != 0
    def left:Boolean = (state2&2) != 0
    def pulsing:Boolean = (state2&4) != 0
}

class Comparator extends RedstoneGatePart(GateType.COMPARATOR) with INeighborTileChangePart with IRedwireEmitter
{
    var lState2:Int = 0

    def state2:Int = lState2&0xFFFFFFFF
    def setState2(i:Int){ lState2 = i }

    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = 0xE

    override def save(tag:CompoundNBT):Unit = {
        super.save(tag)
        tag.putInt("state2", lState2)
    }
    override def load(tag:CompoundNBT):Unit = {
        super.load(tag)
        lState2 = tag.getInt("state2")
    }

    override def gateLogicCycleShape():Boolean = {
        setShape(if (shape > 0) 0 else 1)
        true
    }

    override def gateLogicCanConnect(r:Int) = true

    override def getRedwireSignal(dir:Int):Int = getOutput(toInternal(dir))

    override def getOutput(r:Int):Int =
        if (r == 0) state2&0xFF
        else 0

    def getAnalogInput(r:Int):Int = (getRedstoneInput(r)+16)/17

    def calcInputA:Int = {
        //TODO comparator calculations may not be accurate anymore

        val absDir = Direction.values()(Rotation.rotateSide(side, toAbsolute(2)))
        var pos = tile.getBlockPos.relative(absDir)
        var state = world.getBlockState(pos)

        if (state.hasAnalogOutputSignal)
            return state.getAnalogOutputSignal(world, pos)

        var i = getRedstoneInput(2)

        if (i < 15 && state.isRedstoneConductor(world, pos)) {
            pos = pos.relative(absDir)
            state = world.getBlockState(pos)

            if (state.hasAnalogOutputSignal)
                i = math.max(state.getAnalogOutputSignal(world, pos), i)

            val entityitemframe = findItemFrame(world, absDir, pos)
            if (entityitemframe != null)
                i = math.max(entityitemframe.getAnalogOutput, i)
        }
        i
    }

    /**
      * Copied from BlockRedstoneComparator#findItemFrame(World, EnumFacing, BlockPos)
      */
    private def findItemFrame(world:World, facing:Direction, pos:BlockPos):ItemFrameEntity = {
        val list:util.List[ItemFrameEntity] = world.getEntitiesOfClass[ItemFrameEntity](classOf[ItemFrameEntity], new AxisAlignedBB(pos.getX.toDouble, pos.getY.toDouble, pos.getZ.toDouble,
            (pos.getX + 1).toDouble, (pos.getY + 1).toDouble, (pos.getZ + 1).toDouble), new Predicate[Entity] {
                override def apply(input:Entity):Boolean = input != null && (input.getDirection == facing)
            }
        )
        if (list.size == 1) list.get(0) else null
    }

    def calcInput:Int = getRedstoneInput(1)<<8|calcInputA<<16|getRedstoneInput(3)<<24

    def digitize(analog:Int):Int = {
        var digital = 0
        for (i <- 0 until 4) if ((analog>>i*8&0xFF) > 0) digital |= 1<<i
        digital
    }

    override def gateLogicOnChange():Unit = {
        val oldInput = state2&0xFFFFFF00
        val newInput = calcInput
        if (oldInput != newInput) {
            setState2(state2&0xFF|newInput)
            setState(digitize(newInput|calcOutput)|state&0xF0)
            onInputChange()
        }
        if ((state2&0xFF) != calcOutput) scheduleTick(2)
    }

    def calcOutput:Int =
        if (shape == 0) if (inputA >= inputB) inputA else 0
        else Math.max(inputA - inputB, 0)

    def inputA:Int = state2>>16&0xFF
    def inputB:Int = Math.max(state2>>8&0xFF, state2>>24&0xFF)

    override def gateLogicOnScheduledTick():Unit = {
        val oldOutput = state2&0xFF
        val newOutput = calcOutput
        if (oldOutput != newOutput) {
            setState2(state2&0xFFFFFF00|newOutput)
            setState(state&0xF|digitize(newOutput)<<4)
            onOutputChange(1)
        }
    }

    override def onNeighborTileChanged(side:Direction, weak:Boolean):Unit = {
        if (side.ordinal == Rotation.rotateSide(this.side, toAbsolute(2)))
            onChange()
    }

    override def weakTileChanges() = true
}
