/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.integration

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.multipart.api.part.AnimateTickPart
import codechicken.multipart.init.CBMultipartModContent
import mrtjp.projectred.api.IConnectable
import mrtjp.projectred.core.{Configurator, IRedwireEmitter, TFaceRSAcquisitions}
import net.minecraft.nbt.CompoundNBT
import net.minecraft.util.{Direction, SoundCategory, SoundEvents}

import java.util.Random

abstract class RedstoneGatePart(gateType:GateType) extends GatePart(gateType) with TFaceRSAcquisitions with AnimateTickPart
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

    override def save(tag:CompoundNBT)
    {
        super.save(tag)
        tag.putByte("state", gateState)
    }

    override def load(tag:CompoundNBT)
    {
        super.load(tag)
        gateState = tag.getByte("state")
    }

    override def writeDesc(packet:MCDataOutput)
    {
        super.writeDesc(packet)
        packet.writeByte(gateState)
    }

    override def readDesc(packet:MCDataInput)
    {
        super.readDesc(packet)
        gateState = packet.readByte()
    }

    override def read(packet:MCDataInput, key:Int) = key match
    {
        case 5 =>
            gateState = packet.readByte()
            if (Configurator.staticGates) tile.markRender()
        case _ => super.read(packet, key)
    }

    def sendStateUpdate()
    {
        sendUpdate(5, _.writeByte(gateState))
    }

    def onInputChange()
    {
        tile.setChanged()
        sendStateUpdate()
    }

    def onOutputChange(mask:Int)
    {
        tile.setChanged()
        sendStateUpdate()
        tile.internalPartChange(this)
        notifyExternals(toAbsoluteMask(mask))
    }

    override def strongPowerLevel(side:Int):Int =
    {
        if ((side&6) == (this.side&6)) return 0
        val ir = toInternal(absoluteRot(side))
        if ((outputMask(shape)&1<<ir) != 0) getOutput(ir) else 0
    }

    override def weakPowerLevel(side:Int):Int = strongPowerLevel(side)

    override def canConnectRedstone(side:Int) =
    {
        if ((side&6) == (this.side&6)) false
        else gateLogicCanConnect(toInternal(absoluteRot(side)))
    }

    override def notifyExternals(mask:Int)
    {
        var smask = 0

        for (r <- 0 until 4) if ((mask&1<<r) != 0) {
            val absSide = absoluteDir(r)
            val pos = this.pos.relative(Direction.values()(absSide))

            world.neighborChanged(pos, CBMultipartModContent.blockMultipart, pos)
            for (s <- 0 until 6) if (s != (absSide^1) && (smask&1<<s) == 0)
                world.neighborChanged(pos.relative(Direction.values()(s)), CBMultipartModContent.blockMultipart, pos)

            smask |= 1<<absSide
        }
    }

    def getRedstoneInput(r:Int) =
    {
        val ar = toAbsolute(r)
        if (maskConnectsCorner(ar)) calcCornerSignal(ar)
        else if (maskConnectsStraight(ar)) calcStraightSignal(ar)
        else if (maskConnectsInside(ar)) calcInternalSignal(ar)
        else calcMaxSignal(ar, true, false)
    }

    def getAnalogRedstoneInput(r:Int):Int = {
        (getRedstoneInput(r) + 16) / 17
    }

    override def resolveSignal(part:Any, r:Int) = part match
    {
        case re:IRedwireEmitter => re.getRedwireSignal(r)
        case _ => 0
    }

    override def animateTick(random:Random):Unit = {
        RenderGate.instance().spawnParticles(this, random)
    }

    def tickSound():Unit = {
        if (Configurator.logicGateSounds)
            world.playSound(null, pos, SoundEvents.LEVER_CLICK, SoundCategory.BLOCKS, 0.15F, 0.5f)
    }

    override def gateLogicCanConnectTo(part:IConnectable, r:Int):Boolean = part match {
        case re:IRedwireEmitter => gateLogicCanConnect(r)
        case _ => false
    }

    def gateLogicCanConnect(r:Int):Boolean = ((outputMask(shape)|inputMask(shape))&1<<r) != 0

    def outputMask(shape:Int):Int = 0
    def inputMask(shape:Int):Int = 0

    def getOutput(r:Int):Int = if ((state&0x10<<r) != 0) 15 else 0
    def getInput(mask:Int):Int = {
        var input = 0
        for (r <- 0 until 4) if ((mask&1<<r) != 0 && getRedstoneInput(r) > 0) input |= 1<<r
        input
    }
}
