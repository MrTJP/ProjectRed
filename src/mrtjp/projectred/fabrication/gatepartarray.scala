/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import mrtjp.core.vec.Point
import mrtjp.projectred.transmission.IWirePart
import net.minecraft.nbt.NBTTagCompound
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

trait TArrayGateICPart extends RedstoneGateICPart with IRedwireICPart with TRSPropagatingICPart
{
    def getLogicArray = getLogic[TArrayICGateLogic[TArrayGateICPart]]

    override def getSignal = getLogicArray.getSignal(toInternalMask(propagationMask))
    override def setSignal(signal:Int) = getLogicArray.setSignal(toInternalMask(propagationMask), signal)

    abstract override def updateAndPropagate(prev:CircuitPart, mode:Int)
    {
        val rd = sideDiff(prev)
        var uMask = 0
        for (r <- 0 until 4) if ((rd&1<<r) != 0)
        {
            val pMask = getLogicArray.propogationMask(toInternal(r))
            if (pMask > 0 && (pMask&uMask) != pMask)
            {
                propagationMask = toAbsoluteMask(pMask)
                super.updateAndPropagate(prev, mode)
                uMask |= pMask
            }
        }
        if (uMask == 0) ICPropagator.addNeighborChange(Point(x, y))
        propagationMask = 0xF
    }

    override def propagateOther(mode:Int)
    {
        val nonConn = ~(connMap|connMap>>4|connMap>>8)&0xF
        notify(nonConn&propagationMask)
    }

    def sideDiff(part:CircuitPart):Int =
    {
        if (part.world == null) return 0xF
        val here = Point(x, y)
        val there = Point(part.x, part.y)
        there-here match
        {
            case Point(0, -1) => 1<<0
            case Point(1,  0) => 1<<1
            case Point(0,  1) => 1<<2
            case Point(-1, 0) => 1<<3
            case _ => throw new RuntimeException(s"Circuit array gate tried to propagate from $here to #$there")
        }
    }

    override def calculateSignal:Int =
    {
        val ipmask = toInternalMask(propagationMask)
        if (getLogicArray.overrideSignal(ipmask))
            return getLogicArray.calculateSignal(ipmask)

        var s = 0
        ICPropagator.redwiresProvidePower = false
        def raise(sig:Int){ if (sig > s) s = sig }
        for (r <- 0 until 4) if ((propagationMask&1<<r) != 0) raise(calcSignal(r))
        ICPropagator.redwiresProvidePower = true
        s
    }

    abstract override def onChange()
    {
        super.onChange()
        ICPropagator.propagateTo(this, IWirePart.RISING)
    }

    override def onSignalUpdate()
    {
        world.network.markSave()
        super.onChange()
        getLogicArray.onSignalUpdate()
    }

    override def resolveSignal(part:Any, r:Int) = part match
    {
        case re:IRedwireICPart if re.diminishOnSide(r) => re.getRedwireSignal(r)-1
        case _ => super.resolveSignal(part, r)
    }

    override def getRedwireSignal(r:Int) =
    {
        val ir = toInternal(r)
        val pmask = getLogicArray.propogationMask(ir)
        if (pmask != 0) getLogicArray.getSignal(pmask)
        else getLogicRS.getOutput(this, ir)
    }

    override def canConnectRS(r:Int):Boolean =
    {
        if (super.canConnectRS(r)) return true
        getLogicArray.canConnectRedwire(this, toInternal(r))
    }

    override def rsOutputLevel(r:Int):Int =
    {
        val ir = toInternal(r)
        if ((getLogicArray.redwireMask(shape)&1<<ir) != 0)
            return if (ICPropagator.redwiresProvidePower)
                        getLogicArray.getSignal(getLogicArray.propogationMask(ir)) else 0
        super.rsOutputLevel(r)
    }

    override def diminishOnSide(r:Int) = (getLogicArray.redwireMask(shape)&1<<toInternal(r)) != 0
}

trait TArrayICGateLogic[T <: TArrayGateICPart] extends RedstoneICGateLogic[T]
{
    abstract override def canConnectTo(gate:T, part:CircuitPart, r:Int) = part match
    {
        case re:IRedwireICPart if canConnectRedwire(gate, r) => true
        case _ => super.canConnectTo(gate, part, r)
    }

    def canConnectRedwire(gate:T, r:Int):Boolean = canConnectRedwire(gate.shape, r)
    def canConnectRedwire(shape:Int, r:Int):Boolean = (redwireMask(shape)&1<<r) != 0

    def redwireMask(shape:Int):Int

    def propogationMask(r:Int):Int

    def getSignal(mask:Int):Int
    def setSignal(mask:Int, signal:Int)

    def overrideSignal(mask:Int) = false
    def calculateSignal(mask:Int) = 0

    def canCross = false

    def onSignalUpdate()
}

class ArrayGateICPart extends RedstoneGateICPart with TComplexGateICPart with TArrayGateICPart
{
    private var logic:ArrayGateICLogic = null

    override def getLogic[T] = logic.asInstanceOf[T]

    override def assertLogic()
    {
        if (logic == null) logic = ArrayGateICLogic.create(this, subID)
    }

    override def getPartType = CircuitPartDefs.ArrayGate
}

object ArrayGateICLogic
{
    import ICGateDefinition._
    def create(gate:ArrayGateICPart, subID:Int) = subID match
    {
        case NullCell.ordinal => new NullCell(gate)
        case InvertCell.ordinal => new InvertCell(gate)
        case BufferCell.ordinal => new BufferCell(gate)
        case _ => throw new IllegalArgumentException("Invalid gate subID: "+subID)
    }
}

abstract class ArrayGateICLogic(val gate:ArrayGateICPart) extends RedstoneICGateLogic[ArrayGateICPart] with TArrayICGateLogic[ArrayGateICPart] with TComplexICGateLogic[ArrayGateICPart]

abstract class ArrayGateICLogicCrossing(gate:ArrayGateICPart) extends ArrayGateICLogic(gate)
{
    var signal1:Byte = 0
    var signal2:Byte = 0

    override def redwireMask(shape:Int) = 0xF
    override def propogationMask(r:Int) = if (r%2 == 0) 0x5 else 0xA
    override def inputMask(shape:Int) = 0xF
    override def outputMask(shape:Int) = 0xF

    override def getSignal(mask:Int) = (if (mask == 0x5) signal1 else signal2)&0xFF
    override def setSignal(mask:Int, signal:Int)
    {
        if (mask == 0x5) signal1 = signal.toByte else signal2 = signal.toByte
    }

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setByte("s1", signal1)
        tag.setByte("s2", signal2)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        signal1 = tag.getByte("s1")
        signal2 = tag.getByte("s2")
    }

    override def writeDesc(packet:MCDataOutput)
    {
        super.writeDesc(packet)
        packet.writeByte(signal1)
        packet.writeByte(signal2)
    }

    override def readDesc(packet:MCDataInput)
    {
        super.readDesc(packet)
        signal1 = packet.readByte()
        signal2 = packet.readByte()
    }

    override def read(packet:MCDataInput, key:Int) = key match
    {
        case 11 =>
            signal1 = packet.readByte()
            signal2 = packet.readByte()
        case _ =>
    }

    def sendSignalUpdate(){ gate.writeStreamOf(11).writeByte(signal1).writeByte(signal2) }

    override def onChange(gate:ArrayGateICPart)
    {
        val oldSignal = (gate.state&1) != 0
        val newSignal = signal1 != 0

        if (oldSignal != newSignal)
        {
            gate.setState(gate.state&2|(if (newSignal) 1 else 0))
            gate.onInputChange()
            gate.scheduleTick(0)
        }
    }

    override def scheduledTick(gate:ArrayGateICPart)
    {
        val input = (gate.state&1) != 0
        val oldOutput = (gate.state&2) != 0
        val newOutput = !input

        if (oldOutput != newOutput)
        {
            gate.setState(gate.state&1|(if (newOutput) 2 else 0))
            gate.onOutputChange(0)
            gate.onChange()
        }
    }

    override def onSignalUpdate(){ sendSignalUpdate() }

    override def overrideSignal(mask:Int) = if (mask == 0xA) powerUp else false

    override def calculateSignal(mask:Int) = 255

    def powerUp:Boolean

    @SideOnly(Side.CLIENT)
    override def getRolloverData(gate:ArrayGateICPart, detailLevel:Int) =
    {
        val data = Seq.newBuilder[String]

        if (detailLevel >= 3)
        {
            data += "lower: 0x"+Integer.toHexString(signal1&0xFF)
            data += "upper: 0x"+Integer.toHexString(signal2&0xFF)
        }
        else if (detailLevel >= 2)
        {
            data += "lower: "+(if (signal1 != 0) "high" else "low")
            data += "upper: "+(if (signal2 != 0) "high" else "low")
        }

        super.getRolloverData(gate, detailLevel)++data.result()
    }
}

class NullCell(gate:ArrayGateICPart) extends ArrayGateICLogicCrossing(gate)
{
    override def powerUp = false
}

class InvertCell(gate:ArrayGateICPart) extends ArrayGateICLogicCrossing(gate)
{
    override def powerUp = (gate.state&2) != 0
}

class BufferCell(gate:ArrayGateICPart) extends ArrayGateICLogicCrossing(gate)
{
    override def powerUp = (gate.state&2) == 0
}
