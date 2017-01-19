/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import net.minecraft.nbt.NBTTagCompound

abstract class RedstoneGateICPart extends GateICPart with TICRSAcquisitions with IPoweredCircuitPart
{
    /**
     * Mapped inputs and outputs of the gate.
     * OOOO IIII
     * High nybble is output.
     * Low nybble is input
     */
    private var gateState:Byte = 0

    def state = gateState&0xFF
    def setState(s:Int){ gateState = s.toByte }

    def getLogicRS = getLogic[RedstoneICGateLogic[RedstoneGateICPart]]

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setByte("state", gateState)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        gateState = tag.getByte("state")
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeByte(gateState)
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        gateState = in.readByte()
    }

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 5 => gateState = in.readByte()
        case _ => super.read(in, key)
    }

    def sendStateUpdate()
    {
        writeStreamOf(5).writeByte(gateState)
    }

    def onInputChange()
    {
        sendStateUpdate()
    }

    def onOutputChange(mask:Int)
    {
        world.network.markSave()
        sendStateUpdate()
        notify(toAbsoluteMask(mask))
    }

    override def rsOutputLevel(r:Int):Int =
    {
        val ir = toInternal(r)
        if((getLogicRS.outputMask(shape)&1<<ir) != 0) getLogicRS.getOutput(this, ir) else 0
    }

    override def canConnectRS(r:Int) = getLogicRS.canConnect(this, toInternal(r))

    def getRedstoneInput(r:Int) = calcSignal(toAbsolute(r))

    override def resolveSignal(part:Any, r:Int) = part match
    {
        case re:IICRedwireEmitter => re.getRedwireSignal(r)
        case ip:IPoweredCircuitPart => ip.rsOutputLevel(r)
        case _ => 0
    }
}

abstract class RedstoneICGateLogic[T <: RedstoneGateICPart] extends ICGateLogic[T]
{
    override def canConnectTo(gate:T, part:CircuitPart, r:Int) = part match
    {
        case re:IICRedwireEmitter => canConnect(gate, r)
        case _ => false
    }

    def canConnect(gate:T, r:Int):Boolean = canConnect(gate.shape, r)
    def canConnect(shape:Int, r:Int):Boolean = ((inputMask(shape)|outputMask(shape))&1<<r) != 0

    def outputMask(shape:Int) = 0
    def inputMask(shape:Int) = 0

    def getOutput(gate:T, r:Int) = if ((gate.state&0x10<<r) != 0) 255 else 0
    def getInput(gate:T, mask:Int) =
    {
        var input = 0
        for (r <- 0 until 4) if ((mask&1<<r) != 0 && gate.getRedstoneInput(r) > 0) input |= 1<<r
        input
    }
}