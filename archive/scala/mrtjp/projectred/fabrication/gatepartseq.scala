/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.multipart.handler.MultipartSaveLoad
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.projectred.ProjectRedCore.log
import mrtjp.projectred.core.Configurator
import mrtjp.projectred.core.TFaceOrient._
import net.minecraft.nbt.NBTTagCompound

class SequentialGateICPart extends RedstoneGateICPart with TComplexGateICPart
{
    var logic:SequentialICGateLogic = null

    override def assertLogic()
    {
        if (logic == null) logic = SequentialICGateLogic.create(this, subID)
    }

    override def getLogic[T]:T = logic.asInstanceOf[T]

    override def getPartType = CircuitPartDefs.ComplexGate

    override def readClientPacket(in:MCDataInput, key:Int) = key match
    {
        case 3 => getLogicPrimitive match {
            case t:ITimerGuiLogic => t.setTimerMax(this, t.getTimerMax+in.readShort())
            case _ => log.error("Server IC stream received client packet for incorrect gate type")
        }
        case 4 => getLogicPrimitive match {
            case t:ICounterGuiLogic =>
                val actionID = in.readByte()
                actionID match {
                    case 0 => t.setCounterMax(this, t.getCounterMax+in.readShort())
                    case 1 => t.setCounterIncr(this, t.getCounterIncr+in.readShort())
                    case 2 => t.setCounterDecr(this, t.getCounterDecr+in.readShort())
                    case _ => log.error("Server IC stream received client packet for incorrect gate type")
                }
            case _ => log.error("Server IC stream received client packet for incorrect gate type")
        }
        case _ => super.readClientPacket(in, key)
    }
}

object SequentialICGateLogic
{
    import mrtjp.projectred.fabrication.{ICGateDefinition => defs}

    def create(gate:SequentialGateICPart, subID:Int) = subID match
    {
        case defs.SRLatch.ordinal => new SRLatch(gate)
        case defs.ToggleLatch.ordinal => new ToggleLatch(gate)
        case defs.Timer.ordinal => new Timer(gate)
        case defs.Sequencer.ordinal => new Sequencer(gate)
        case defs.Counter.ordinal => new Counter(gate)
        case defs.StateCell.ordinal => new StateCell(gate)
        case defs.Synchronizer.ordinal => new Synchronizer(gate)
        case _ => throw new IllegalArgumentException("Invalid gate subID: "+subID)
    }
}

abstract class SequentialICGateLogic(val gate:SequentialGateICPart) extends RedstoneICGateLogic[SequentialGateICPart] with TComplexICGateLogic[SequentialGateICPart]

trait TExtraStateLogic extends SequentialICGateLogic
{
    private var lState2:Byte = 0

    def state2 = lState2&0xFF
    def setState2(state:Int){ lState2 = state.toByte }

    def clientState2 = false

    abstract override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setByte("state2", lState2)
    }

    abstract override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        lState2 = tag.getByte("state2")
    }

    abstract override def writeDesc(packet:MCDataOutput)
    {
        super.writeDesc(packet)
        if (clientState2) packet.writeByte(lState2)
    }

    abstract override def readDesc(packet:MCDataInput)
    {
        super.readDesc(packet)
        if (clientState2) lState2 = packet.readByte()
    }

    abstract override def read(packet:MCDataInput, key:Int) = key match
    {
        case 11 => lState2 = packet.readByte()
        case _ => super.read(packet, key)
    }

    def sendState2Update(){ gate.writeStreamOf(11).writeByte(lState2) }
}

class SRLatch(gate:SequentialGateICPart) extends SequentialICGateLogic(gate) with TExtraStateLogic
{
    override def outputMask(shape:Int) = if ((shape>>1) == 0) 0xF else 5
    override def inputMask(shape:Int) = 0xA

    override def cycleShape(gate:SequentialGateICPart) =
    {
        gate.setShape((gate.shape+1)%4)
        setState2(flipMaskZ(state2))
        gate.setState(flipMaskZ(gate.state))
        gate.onOutputChange(0xF)
        gate.scheduleTick(0)
        true
    }

    override def setup(gate:SequentialGateICPart)
    {
        setState2(2)
        gate.setState(0x30)
    }

    override def onChange(gate:SequentialGateICPart)
    {
        val stateInput = state2

        val oldInput = gate.state&0xF
        val newInput = getInput(gate, 0xA)
        val oldOutput = gate.state>>4

        if (newInput != oldInput)
            if (stateInput != 0xA && newInput != 0 && newInput != stateInput) //state needs changing
            {
                gate.setState(newInput)
                setState2(newInput)
                gate.onOutputChange(oldOutput) //always going low
                gate.scheduleTick(0)
            }
            else
            {
                gate.setState(oldOutput<<4|newInput)
                gate.onInputChange()
            }
    }

    override def scheduledTick(gate:SequentialGateICPart)
    {
        val oldOutput = gate.state>>4
        val newOutput = calcOutput(gate)

        if (oldOutput != newOutput)
        {
            gate.setState(gate.state&0xF|newOutput<<4)
            gate.onOutputChange(outputMask(gate.shape))
        }
        onChange(gate)
    }

    def calcOutput(gate:SequentialGateICPart):Int =
    {
        var input = gate.state&0xF
        var stateInput = state2

        if ((gate.shape&1) != 0) //reverse
        {
            input = flipMaskZ(input)
            stateInput = flipMaskZ(stateInput)
        }

        if (stateInput == 0xA) //disabled
        {
            if (input == 0xA)
            {
                gate.scheduleTick(0)
                return 0
            }

            stateInput =
                    if (input == 0) if (gate.world.network.getWorld.rand.nextBoolean()) 2 else 8
                    else input

            setState2(if ((gate.shape&1) != 0) flipMaskZ(stateInput) else stateInput)
        }

        var output = shiftMask(stateInput, 1)
        if ((gate.shape&2) == 0) output |= stateInput
        if ((gate.shape&1) != 0) output = flipMaskZ(output) //reverse
        output
    }
}

class ToggleLatch(gate:SequentialGateICPart) extends SequentialICGateLogic(gate) with TExtraStateLogic
{
    override def outputMask(shape:Int) = 5
    override def inputMask(shape:Int) = 0xA

    override def clientState2 = true

    override def setup(gate:SequentialGateICPart)
    {
        gate.setState(0x10)
        gate.sendStateUpdate()
    }

    override def onChange(gate:SequentialGateICPart)
    {
        val oldInput = gate.state&0xF
        val newInput = getInput(gate, 0xA)
        val high = newInput& ~oldInput

        if (high == 2 || high == 8) toggle(gate)

        if (oldInput != newInput)
        {
            gate.setState(gate.state&0xF0|newInput)
            gate.onInputChange()
        }
    }

    override def scheduledTick(gate:SequentialGateICPart)
    {
        val oldOutput = gate.state>>4
        val newOutput = if (state2 == 0) 1 else 4
        if (oldOutput != newOutput)
        {
            gate.setState(newOutput<<4|gate.state&0xF)
            gate.onOutputChange(5)
        }
        onChange(gate)
    }

    override def activate(gate:SequentialGateICPart)
    {
        toggle(gate)
    }

    def toggle(gate:SequentialGateICPart)
    {
        setState2(state2^1)
        gate.scheduleTick(0)
    }
}

trait ITimerGuiLogic
{
    def getTimerMax:Int
    def setTimerMax(gate:GateICPart, t:Int)
}

trait ICounterGuiLogic
{
    def getCounterMax:Int
    def setCounterMax(gate:GateICPart, i:Int)

    def getCounterIncr:Int
    def setCounterIncr(gate:GateICPart, i:Int)

    def getCounterDecr:Int
    def setCounterDecr(gate:GateICPart, i:Int)

    def getCounterValue:Int
    def setCounterValue(gate:GateICPart, i:Int)
}

trait TTimerGateLogic extends SequentialICGateLogic with ITimerGuiLogic
{
    var pointer_max = 38
    var pointer_start = -1L
    var saveTime = -1L //used for blueprint in-hand rendering

    abstract override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setInteger("pmax", pointer_max)
        tag.setLong("pelapsed", if (pointer_start < 0) pointer_start else getTotalTime-pointer_start)
        tag.setLong("tsave", getTotalTime)
    }

    abstract override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        pointer_max = tag.getInteger("pmax")
        pointer_start = tag.getLong("pelapsed")
        saveTime = tag.getLong("tsave")
        if (pointer_start >= 0) pointer_start = getTotalTime-pointer_start
    }

    abstract override def writeDesc(packet:MCDataOutput)
    {
        super.writeDesc(packet)
        packet.writeInt(pointer_max)
        packet.writeLong(pointer_start)
    }

    abstract override def readDesc(packet:MCDataInput)
    {
        super.readDesc(packet)
        pointer_max = packet.readInt()
        pointer_start = packet.readLong()
    }

    abstract override def read(packet:MCDataInput, key:Int) = key match
    {
        case 12 => pointer_max = packet.readInt()
        case 13 =>
            pointer_start = packet.readInt()
            if (pointer_start >= 0) pointer_start = getTotalTime-pointer_start
        case _ => super.read(packet, key)
    }

    def getTotalTime = //client-side safe version of getTotalWorldTime (workaround for no client world)
    {
        if (gate.world.network == null) saveTime //ic was loaded directly from stack, possibly for in-hand render
        else if (gate.world.network.getWorld == null) MultipartSaveLoad.loadingWorld.getTotalWorldTime //ic is being loaded with a workbench tile or gate
        else gate.world.network.getWorld.getTotalWorldTime //normal access during operation
    }

    def pointerValue = if (pointer_start < 0) 0 else (getTotalTime-pointer_start).toInt

    def sendPointerMaxUpdate(){ gate.writeStreamOf(12).writeInt(pointer_max) }
    def sendPointerUpdate(){ gate.writeStreamOf(13).writeInt(if (pointer_start < 0) -1 else pointerValue)}

    override def getTimerMax = pointer_max+2
    override def setTimerMax(gate:GateICPart, time:Int)
    {
        var t = time
        val minTime = math.max(4, Configurator.minTimerTicks)
        if (t < minTime) t = minTime
        if (t != pointer_max)
        {
            pointer_max = t-2
            sendPointerMaxUpdate()
        }
    }

    override def onTick(gate:SequentialGateICPart)
    {
        if (pointer_start >= 0)
            if (getTotalTime >= pointer_start+pointer_max) pointerTick()
            else if (pointer_start > getTotalTime)
                pointer_start = getTotalTime
    }

    def pointerTick()

    def resetPointer()
    {
        if (pointer_start >= 0)
        {
            pointer_start = -1
            gate.world.network.markSave()
            if (!gate.world.network.isRemote) sendPointerUpdate()
        }
    }

    def startPointer()
    {
        if (pointer_start < 0)
        {
            pointer_start = getTotalTime
            gate.world.network.markSave()
            if (!gate.world.network.isRemote) sendPointerUpdate()
        }
    }

    def interpPointer(f:Float) = if (pointer_start < 0) 0f else (pointerValue+f)/pointer_max

    @SideOnly(Side.CLIENT)
    override def createGui(gate:SequentialGateICPart):CircuitGui = new ICTimerGateGui(gate)

    @SideOnly(Side.CLIENT)
    override def getRolloverData(gate:SequentialGateICPart, detailLevel:Int) =
    {
        val data = Seq.newBuilder[String]
        if (detailLevel > 1) data += "interval: "+"%.2f".format(getTimerMax*0.05)+"s"
        super.getRolloverData(gate, detailLevel)++data.result()
    }
}

class Timer(gate:SequentialGateICPart) extends SequentialICGateLogic(gate) with TTimerGateLogic
{
    override def outputMask(shape:Int) = 0xB
    override def inputMask(shape:Int) = 0xE

    override def setup(gate:SequentialGateICPart){ startPointer() }

    override def scheduledTick(gate:SequentialGateICPart)
    {
        gate.setState(gate.state&0xF)
        gate.onOutputChange(0xB)
        onChange(gate)
    }

    override def onChange(gate:SequentialGateICPart)
    {
        val oldInput = gate.state&0xF
        val newInput = getInput(gate, 0xE)

        if (newInput != oldInput)
        {
            gate.setState(gate.state&0xF0|newInput)
            gate.onInputChange()
        }

        if (gate.schedTime < 0)
            if (newInput > 0) resetPointer() else startPointer()
    }

    override def pointerTick()
    {
        resetPointer()
        if (!gate.world.network.isRemote)
        {
            gate.scheduleTick(2)
            gate.setState(0xB0|gate.state&0xF)
            gate.onOutputChange(0xB)
        }
    }
}

class Sequencer(gate:SequentialGateICPart) extends SequentialICGateLogic(gate) with ITimerGuiLogic
{
    var pointer_max = 40
    var saveTime = -1L

    override def outputMask(shape:Int) = 0xF

    override def onChange(gate:SequentialGateICPart){}
    override def scheduledTick(gate:SequentialGateICPart){}

    override def getTimerMax = pointer_max
    override def setTimerMax(gate:GateICPart, time:Int)
    {
        var t = time
        val minTime = math.max(4, Configurator.minTimerTicks)
        if (t < minTime) t = minTime
        if (t != pointer_max)
        {
            pointer_max = t
            sendPointerMaxUpdate()
        }
    }

    override def save(tag:NBTTagCompound)
    {
        tag.setInteger("pmax", pointer_max)
        tag.setLong("tsave", getWorldTime)
    }
    override def load(tag:NBTTagCompound)
    {
        pointer_max = tag.getInteger("pmax")
        saveTime = tag.getLong("tsave")
    }

    override def writeDesc(packet:MCDataOutput){ packet.writeInt(pointer_max) }
    override def readDesc(packet:MCDataInput){ pointer_max = packet.readInt() }

    override def read(packet:MCDataInput, key:Int) = key match
    {
        case 12 => pointer_max = packet.readInt()
        case _ =>
    }

    def sendPointerMaxUpdate(){ gate.writeStreamOf(12).writeInt(pointer_max) }

    def getWorldTime =
        if (gate.world.network != null) gate.world.network.getWorld.getWorldTime
        else saveTime

    override def onTick(gate:SequentialGateICPart)
    {
        if (!gate.world.network.isRemote)
        {
            val oldOut = gate.state>>4
            var out = 1<<getWorldTime%(pointer_max*4)/pointer_max
            if (gate.shape == 1) out = flipMaskZ(out)
            if (oldOut != out)
            {
                gate.setState(out<<4)
                gate.onOutputChange(0xF)
            }
        }
    }

    override def cycleShape(gate:SequentialGateICPart) =
    {
        gate.setShape(gate.shape^1)
        true
    }

    @SideOnly(Side.CLIENT)
    override def createGui(gate:SequentialGateICPart):CircuitGui = new ICTimerGateGui(gate)

    @SideOnly(Side.CLIENT)
    override def getRolloverData(gate:SequentialGateICPart, detailLevel:Int) =
    {
        val data = Seq.newBuilder[String]
        if (detailLevel > 1) data += "interval: "+"%.2f".format(getTimerMax*0.05)+"s"
        super.getRolloverData(gate, detailLevel)++data.result()
    }
}

class Counter(gate:SequentialGateICPart) extends SequentialICGateLogic(gate) with ICounterGuiLogic
{
    var value = 0
    var max = 10
    var incr = 1
    var decr = 1

    override def outputMask(shape:Int) = 5
    override def inputMask(shape:Int) = 10

    override def save(tag:NBTTagCompound)
    {
        tag.setInteger("val", value)
        tag.setInteger("max", max)
        tag.setInteger("inc", incr)
        tag.setInteger("dec", decr)
    }

    override def load(tag:NBTTagCompound)
    {
        value = tag.getInteger("val")
        max = tag.getInteger("max")
        incr = tag.getInteger("inc")
        decr = tag.getInteger("dec")
    }

    override def writeDesc(packet:MCDataOutput)
    {
        packet.writeInt(value).writeInt(max).writeInt(incr).writeInt(decr)
    }

    override def readDesc(packet:MCDataInput)
    {
        value = packet.readInt()
        max = packet.readInt()
        incr = packet.readInt()
        decr = packet.readInt()
    }

    override def read(packet:MCDataInput, key:Int) = key match
    {
        case 11 => value = packet.readInt()
        case 12 => max = packet.readInt()
        case 13 => incr = packet.readInt()
        case 14 => decr = packet.readInt()
        case _ =>
    }

    def sendValueUpdate(){ gate.writeStreamOf(11).writeInt(value) }
    def sendMaxUpdate(){ gate.writeStreamOf(12).writeInt(max) }
    def sendIncrUpdate(){ gate.writeStreamOf(13).writeInt(incr) }
    def sendDecrUpdate(){ gate.writeStreamOf(14).writeInt(decr) }

    override def getCounterValue = value
    override def getCounterMax = max
    override def getCounterIncr = incr
    override def getCounterDecr = decr

    override def setCounterValue(gate:GateICPart, i:Int)
    {
        val oldVal = value
        value = Math.min(max, Math.max(0, i))
        if (value != oldVal)
            sendValueUpdate()
    }

    override def setCounterMax(gate:GateICPart, i:Int)
    {
        val oldMax = max
        max = Math.min(32767, Math.max(1, i))
        if (max != oldMax)
        {
            sendMaxUpdate()
            val oldVal = value
            value = Math.min(value, Math.max(0, i))
            if (value != oldVal)
            {
                sendValueUpdate()
                gate.scheduleTick(2)
            }
        }
    }

    override def setCounterIncr(gate:GateICPart, i:Int)
    {
        val oldIncr = incr
        incr = Math.min(max, Math.max(1, i))
        if (incr != oldIncr)
            sendIncrUpdate()
    }

    override def setCounterDecr(gate:GateICPart, i:Int)
    {
        val oldDecr = decr
        decr = Math.min(max, Math.max(1, i))
        if (decr != oldDecr)
            sendDecrUpdate()
    }

    def onChange(gate:SequentialGateICPart)
    {
        val oldInput = gate.state&0xF
        var newInput = getInput(gate, 0xA)
        if (gate.shape == 1) newInput = flipMaskZ(newInput)
        val high = newInput& ~oldInput

        if ((high&2) != 0) setCounterValue(gate, value+incr)
        if ((high&8) != 0) setCounterValue(gate, value-decr)
        if (oldInput != newInput)
        {
            gate.setState(gate.state&0xF0|newInput)
            gate.onInputChange()
            gate.scheduleTick(2)
        }
    }

    override def cycleShape(gate:SequentialGateICPart) =
    {
        gate.setShape(if (gate.shape == 1) 0 else 1)
        true
    }

    def scheduledTick(gate:SequentialGateICPart)
    {
        val oldOutput = gate.state
        var newOutput = 0
        if (value == max) newOutput = 1
        else if (value == 0) newOutput = 4
        if (newOutput != oldOutput) gate.setState(gate.state&0xF|newOutput<<4)
        if (newOutput != oldOutput) gate.onOutputChange(5)
    }

    @SideOnly(Side.CLIENT)
    override def createGui(gate:SequentialGateICPart):CircuitGui = new ICCounterGateGui(gate)

    @SideOnly(Side.CLIENT)
    override def getRolloverData(gate:SequentialGateICPart, detailLevel:Int) =
    {
        val data = Seq.newBuilder[String]
        if (detailLevel > 1)
        {
            data += "state: "+getCounterValue
            if (detailLevel > 2)
            {
                data += "max: "+getCounterMax
                data += "incr: "+getCounterIncr
                data += "decr: "+getCounterDecr
            }
        }
        super.getRolloverData(gate, detailLevel)++data.result()
    }
}

class StateCell(gate:SequentialGateICPart) extends SequentialICGateLogic(gate) with TTimerGateLogic with TExtraStateLogic
{
    override def outputMask(shape:Int) =
    {
        var output = 9
        if (shape == 1) output = flipMaskZ(output)
        output
    }

    override def inputMask(shape:Int) =
    {
        var input = 6
        if (shape == 1) input = flipMaskZ(input)
        input
    }

    override def cycleShape(gate:SequentialGateICPart) =
    {
        gate.setShape((gate.shape+1)%2)
        true
    }

    override def onChange(gate:SequentialGateICPart)
    {
        val oldInput = gate.state&0xF
        var newInput = getInput(gate, 0xE)
        if (oldInput != newInput)
        {
            gate.setState(gate.state&0xF0|newInput)
            gate.onInputChange()

            if (gate.shape == 1) newInput = flipMaskZ(newInput)
            if ((newInput&4) != 0 && state2 == 0)
            {
                setState2(1)
                sendState2Update()
                gate.scheduleTick(0)
            }

            if (state2 != 0) if ((newInput&6) != 0) resetPointer()
            else startPointer()
        }
    }

    override def pointerTick()
    {
        resetPointer()
        if (!gate.world.network.isRemote)
        {
            setState2(0)
            sendState2Update()
            gate.setState(0x10|gate.state&0xF)
            gate.onOutputChange(outputMask(gate.shape))
            gate.scheduleTick(2)
        }
    }

    override def scheduledTick(gate:SequentialGateICPart)
    {
        var output = 0
        if (state2 != 0) output = 8
        if (gate.shape == 1) output = flipMaskZ(output)

        gate.setState(output<<4|gate.state&0xF)
        gate.onOutputChange(outputMask(gate.shape))
    }
}

class Synchronizer(gate:SequentialGateICPart) extends SequentialICGateLogic(gate) with TExtraStateLogic
{
    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = 14

    override def onChange(gate:SequentialGateICPart)
    {
        val oldInput = gate.state&0xF
        val newInput = getInput(gate, 14)
        val high = newInput& ~oldInput
        if (oldInput != newInput)
        {
            val oldValue = state2

            gate.setState(gate.state&0xF0|newInput)
            gate.onInputChange()
            if ((newInput&4) != 0) setState2(0)
            else
            {
                if ((high&2) != 0) setState2(state2|1) //right
                if ((high&8) != 0) setState2(state2|2) //left
            }
            if (right && left) gate.scheduleTick(0)

            if (state2 != oldValue) sendState2Update()
        }
    }

    override def scheduledTick(gate:SequentialGateICPart)
    {
        val oldValue = state2
        if (!pulsing && right && left)
        {
            gate.setState(gate.state|1<<4)
            gate.onOutputChange(1)
            setState2(state2|4) //pulsing
            gate.scheduleTick(2)
        }
        else if (pulsing)
        {
            gate.setState(gate.state& ~0x10)
            gate.onOutputChange(1)
            setState2(0) //off
        }
        if (state2 != oldValue) sendState2Update()
    }

    def right = (state2&1) != 0
    def left = (state2&2) != 0
    def pulsing = (state2&4) != 0

    @SideOnly(Side.CLIENT)
    override def getRolloverData(gate:SequentialGateICPart, detailLevel:Int) =
    {
        val data = Seq.newBuilder[String]
        if (detailLevel > 1)
        {
            data += "0: "+(if (right) "high" else "low")
            data += "1: "+(if (left) "high" else "low")
        }
        super.getRolloverData(gate, detailLevel)++data.result()
    }
}