/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import net.minecraft.nbt.NBTTagCompound

trait IRedwireICGate
{
    def canConnectRS(r:Int):Boolean =
        canOutputTo(r) || canInputFrom(r)

    def canOutputTo(r:Int):Boolean

    def canInputFrom(r:Int):Boolean
}

abstract class RedstoneGateICTile extends GateICTile with IRedwireICGate
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

    def getLogicRS = getLogic[RedstoneGateTileLogic[RedstoneGateICTile]]

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

    override def canOutputTo(r:Int) = getLogicRS.canOutput(this, toInternal(r))

    override def canInputFrom(r:Int) = getLogicRS.canInput(this, toInternal(r))
}

abstract class RedstoneGateTileLogic[T <: RedstoneGateICTile] extends GateTileLogic[T]
{
    override def canConnectTo(gate:T, part:ICTile, r:Int) = part match
    {
        case re:IICRedwireEmitter => canConnect(gate, r)
        case _ => false
    }

    def canConnect(gate:T, r:Int):Boolean = canOutput(gate, r) || canInput(gate, r)

    def canOutput(gate:T, r:Int):Boolean = (outputMask(gate.shape)&1<<r) != 0
    def canInput(gate:T, r:Int):Boolean = (inputMask(gate.shape)&1<<r) != 0

    def outputMask(shape:Int) = 0
    def inputMask(shape:Int) = 0
}