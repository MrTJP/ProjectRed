/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.integration

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.raytracer.VoxelShapeCache
import codechicken.lib.vec._
import codechicken.multipart.api.part.TMultiPart
import codechicken.multipart.block.BlockMultiPart
import com.google.common.collect.ImmutableSet
import mrtjp.projectred.api.IConnectable
import mrtjp.projectred.core._
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.nbt.CompoundNBT
import net.minecraft.util.Direction
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.shapes.{ISelectionContext, VoxelShape}

abstract class ArrayGatePart(gateType:GateType) extends RedstoneGatePart(gateType) with IRedwirePart with TFaceRSPropagation
{
    override def getSignal:Int = getSignal(toInternalMask(propagationMask))
    override def setSignal(signal:Int):Unit = setSignal(toInternalMask(propagationMask), signal)

    override def updateAndPropagate(prev:TMultiPart, mode:Int):Unit = {
        val rd = sideDiff(prev)
        var uMask = 0
        for (r <- 0 until 4) if ((rd&1<<r) != 0) {
            val pMask = propogationMask(toInternal(r))
            if (pMask > 0 && (pMask&uMask) != pMask) {
                propagationMask = toAbsoluteMask(pMask)
                super.updateAndPropagate(prev, mode)
                uMask |= pMask
            }
        }
        if (uMask == 0) WirePropagator.addNeighborChange(pos)
        propagationMask = 0xF
    }

    override def propagateOther(mode:Int):Unit = {
        val nonConn = ~(connMap|connMap>>4|connMap>>8)&0xF
        notifyExternals(nonConn&propagationMask)
    }

    def sideDiff(p:TMultiPart):Int = {
        if (!p.isInstanceOf[TFaceOrient] || p.tile == null) return 0xF
        val part = p.asInstanceOf[TFaceOrient]
        val here = pos
        val there = part.pos.mutable()

        if (here == there && (side&6) != (part.side&6)) return 1<<Rotation.rotationTo(side, part.side)

        if (side != part.side) there.move(Direction.values()(side^1)) //bring corner up to same plane

        import codechicken.lib.vec.Rotation._
        (here.getX-there.getX, here.getY-there.getY, here.getZ-there.getZ) match {
            case ( 0, 1, 0) => 1<<rotationTo(side, 0)
            case ( 0,-1, 0) => 1<<rotationTo(side, 1)
            case ( 0, 0, 1) => 1<<rotationTo(side, 2)
            case ( 0, 0,-1) => 1<<rotationTo(side, 3)
            case ( 1, 0, 0) => 1<<rotationTo(side, 4)
            case (-1, 0, 0) => 1<<rotationTo(side, 5)
            case _ => throw new RuntimeException("Propagating to distant part from "+here+" to "+there+"!?")
        }
    }

    override def calculateSignal:Int = {
        val ipmask = toInternalMask(propagationMask)
        if (overrideSignal(ipmask))
            return calculateSignal(ipmask)

        WirePropagator.setDustProvidePower(false)
        WirePropagator.redwiresProvidePower = false
        var s = 0
        def raise(sig:Int){ if (sig > s) s = sig }

        for (r <- 0 until 4) if ((propagationMask&1<<r) != 0)
            if (maskConnectsCorner(r)) raise(calcCornerSignal(r))
            else if (maskConnectsStraight(r)) raise(calcStraightSignal(r))
            else if (maskConnectsInside(r)) raise(calcInternalSignal(r))
            else raise(calcMaxSignal(r, true, true))

        WirePropagator.setDustProvidePower(true)
        WirePropagator.redwiresProvidePower = true
        s
    }

    override def onChange():Unit = {
        super.onChange()
        WirePropagator.propagateTo(this, IWirePart.RISING)
    }

    override def onSignalUpdate():Unit = {
        tile.setChanged()
        super.onChange()
    }

    override def resolveSignal(part:Any, r:Int):Int = part match {
        case re:IRedwirePart if re.diminishOnSide(r) => re.getRedwireSignal(r)-1
        case _ => super.resolveSignal(part, r)
    }

    override def getRedwireSignal(r:Int):Int = {
        val ir = toInternal(r)
        val pmask = propogationMask(ir)
        if (pmask != 0) getSignal(pmask)
        else getOutput(ir)*17
    }

    override def canConnectRedstone(side:Int):Boolean = {
        if (super.canConnectRedstone(side)) true
        else if ((side&6) == (this.side&6)) false
        else canConnectRedwire(toInternal(absoluteRot(side)))
    }

    def rsLevel(i:Int):Int =
        if (WirePropagator.redwiresProvidePower) (i+16)/17
        else 0

    override def weakPowerLevel(side:Int):Int = {
        if ((side&6) == (this.side&6)) 0
        else {
            val ir = toInternal(absoluteRot(side))
            if ((redwireMask(shape)&1<<ir) != 0)
                rsLevel(getSignal(propogationMask(ir)))
            else
                super.weakPowerLevel(side)
        }
    }

    override def diminishOnSide(r:Int):Boolean = (redwireMask(shape)&1<<toInternal(r)) != 0

    override def rotate():Unit = {
        val r = rotation
        setRotation((r+1)%4)
        val b = tile.canReplacePart(this, this)
        setRotation(r)
        if (b) super.rotate()
    }

    override def preparePlacement(player:PlayerEntity, pos:BlockPos, side:Int):Unit = {
        super.preparePlacement(player, pos, side)
        if (canCross) {
            val npart = BlockMultiPart.getPart(player.level, pos, this.side^1)
            npart match {
                case apart:ArrayGatePart => if (apart.getGateType == getGateType && (apart.rotation&1) == (rotation&1))
                    setRotation((rotation+1)%4)
                case _ =>
            }
        }
    }

    override def occlusionTest(npart:TMultiPart) = npart match {
        case apart:ArrayGatePart if apart.canCross =>
            if (apart.getGateType == getGateType && apart.side == (side^1) && (apart.rotation&1) != (rotation&1)) true
            else super.occlusionTest(npart)
        case _ => super.occlusionTest(npart)
    }

    override def gateLogicCanConnectTo(part:IConnectable, r:Int):Boolean = part match {
        case re:IRedwirePart if canConnectRedwire(r) => true
        case _ => super.gateLogicCanConnectTo(part, r)
    }

    def canConnectRedwire(r:Int):Boolean = (redwireMask(shape)&1<<r) != 0

    def redwireMask(shape:Int):Int

    def propogationMask(r:Int):Int

    def getSignal(mask:Int):Int
    def setSignal(mask:Int, signal:Int)

    def overrideSignal(mask:Int) = false
    def calculateSignal(mask:Int) = 0

    def canCross = false
}

object IGateWireRenderConnect
{
    def getConnsAtHeight(gate:GatePart, h:Double) = {
        var conn = 0
        for (r <- 0 until 4) if (getConnHeight(gate, r) == h) conn |= 1<<r
        gate.toInternalMask(conn)
    }

    def getConnHeight(gate:GatePart, r:Int):Double = gate.getStraight(r) match {
        case g:GatePart with IGateWireRenderConnect =>
            val ir = g.toInternal(gate.rotFromStraight(r))
            if ((g.renderConnectMask&1<<ir) != 0) g.getHeight(ir)
            else -1.0D
        case _ => -1.0D
    }
}

trait IGateWireRenderConnect
{
    def renderConnectMask:Int
    def getHeight(r:Int):Double
}

object ArrayGatePart
{
    val oBoxes = Array.ofDim[Cuboid6](6, 2)
    val cBoxes = new Array[Cuboid6](6)
    val oShapes = new Array[VoxelShape](6)
    val cShapes = new Array[VoxelShape](6)

    for (s <- 0 until 6) {
        val occlusion1 = new Cuboid6(1/8D, 0, 0, 7/8D, 6/8D, 1)
        val occlusion2 = new Cuboid6(0, 0, 1/8D, 1, 6/8D, 7/8D)
        val collision = new Cuboid6(0, 0, 0, 1, 6/8D, 1)

        val t = Rotation.sideRotations(s).at(Vector3.CENTER)
        oBoxes(s)(0) = occlusion1.apply(t)
        oBoxes(s)(1) = occlusion2.apply(t)
        cBoxes(s) = collision.apply(t)

        oShapes(s) = VoxelShapeCache.merge(ImmutableSet.copyOf(oBoxes(s).map(VoxelShapeCache.getShape)))
        cShapes(s) = VoxelShapeCache.getShape(cBoxes(s))
    }
}

abstract class ArrayGatePartCrossing(gateType:GateType) extends ArrayGatePart(gateType) with IGateWireRenderConnect
{
    var signal1:Byte = 0
    var signal2:Byte = 0

    override def redwireMask(shape:Int) = 0xF
    override def propogationMask(r:Int) = if (r%2 == 0) 0x5 else 0xA
    override def inputMask(shape:Int) = 0xF
    override def outputMask(shape:Int) = 0xF

    override def renderConnectMask = 0xA
    override def getHeight(r:Int) = 10.0D

    override def getSignal(mask:Int) = (if (mask == 0x5) signal1 else signal2)&0xFF
    override def setSignal(mask:Int, signal:Int)
    {
        if (mask == 0x5) signal1 = signal.toByte else signal2 = signal.toByte
    }

    override def save(tag:CompoundNBT):Unit = {
        super.save(tag)
        tag.putByte("s1", signal1)
        tag.putByte("s2", signal2)
    }

    override def load(tag:CompoundNBT):Unit = {
        super.load(tag)
        signal1 = tag.getByte("s1")
        signal2 = tag.getByte("s2")
    }

    override def writeDesc(packet:MCDataOutput):Unit = {
        super.writeDesc(packet)
        packet.writeByte(signal1)
        packet.writeByte(signal2)
    }

    override def readDesc(packet:MCDataInput):Unit = {
        super.readDesc(packet)
        signal1 = packet.readByte()
        signal2 = packet.readByte()
    }

    override def read(packet:MCDataInput, key:Int):Unit = key match {
        case 11 =>
            signal1 = packet.readByte()
            signal2 = packet.readByte()
            if (Configurator.staticGates) tile.markRender()
        case _ => super.read(packet, key)
    }

    def sendSignalUpdate():Unit = { sendUpdate(11, _.writeByte(signal1).writeByte(signal2)) }

    override def gateLogicOnChange():Unit = {
        val oldSignal = (state&1) != 0
        val newSignal = signal1 != 0

        if (oldSignal != newSignal) {
            setState(state&2|(if (newSignal) 1 else 0))
            onInputChange()
            scheduleTick(2)
        }
    }

    override def gateLogicOnScheduledTick():Unit = {
        val input = (state&1) != 0
        val oldOutput = (state&2) != 0
        val newOutput = !input

        if (oldOutput != newOutput) {
            setState(state&1|(if (newOutput) 2 else 0))
            onOutputChange(0)
            onChange()
        }
    }

    override def getShape(context:ISelectionContext):VoxelShape = getCollisionShape(context)
    override def getCollisionShape(context:ISelectionContext):VoxelShape = ArrayGatePart.cShapes(side)
    override def getOcclusionShape:VoxelShape = ArrayGatePart.oShapes(side)

    override def onSignalUpdate():Unit = {
        super.onSignalUpdate()
        sendSignalUpdate()
    }

    override def overrideSignal(mask:Int):Boolean = if (mask == 0xA) powerUp else false

    override def calculateSignal(mask:Int) = 255

    def powerUp:Boolean
}

class NullCell extends ArrayGatePartCrossing(GateType.NULL_CELL)
{
    override def canCross = true

    override def powerUp = false

    override def getLightValue:Int = 0
}

class InvertCell extends ArrayGatePartCrossing(GateType.INVERT_CELL)
{
    override def powerUp:Boolean = (state&2) != 0
}

class BufferCell extends ArrayGatePartCrossing(GateType.BUFFER_CELL)
{
    override def powerUp:Boolean = (state&2) == 0
}

abstract class ArrayGatePartTopOnly(gateType:GateType) extends ArrayGatePart(gateType)
{
    var signal:Byte = 0

    override def redwireMask(shape:Int) = 0xA
    override def propogationMask(r:Int):Int = if (r%2 == 1) 0xA else 0

    override def getSignal(mask:Int):Int = if (mask == 0xA) signal&0xFF else 0
    override def setSignal(mask:Int, sig:Int):Unit = { if (mask == 0xA) signal = sig.toByte }

    override def save(tag:CompoundNBT):Unit = {
        super.save(tag)
        tag.putByte("signal", signal)
    }

    override def load(tag:CompoundNBT):Unit = {
        super.load(tag)
        signal = tag.getByte("signal")
    }

    override def writeDesc(packet:MCDataOutput):Unit = {
        super.writeDesc(packet)
        packet.writeByte(signal)
    }

    override def readDesc(packet:MCDataInput):Unit = {
        super.readDesc(packet)
        signal = packet.readByte()
    }

    override def read(packet:MCDataInput, key:Int):Unit = key match {
        case 11 =>
            signal = packet.readByte()
            if (Configurator.staticGates) tile.markRender()
        case _ => super.read(packet, key)
    }

    def sendSignalUpdate():Unit = { sendUpdate(11, _.writeByte(signal)) }

    override def onSignalUpdate():Unit = {
        super.onSignalUpdate()
        sendSignalUpdate()
    }
}

class ANDCell extends ArrayGatePartTopOnly(GateType.AND_CELL) with TSimpleRSGatePart with IGateWireRenderConnect
{
    override def inputMask(shape:Int) = 4
    override def outputMask(shape:Int) = 1

    override def renderConnectMask = 0xA
    override def getHeight(r:Int) = 10.0D

    override def calcOutput(input:Int):Int = if (input == 4 && signal != 0) 1 else 0

    override def getShape(context:ISelectionContext):VoxelShape = getCollisionShape(context)
    override def getCollisionShape(context:ISelectionContext):VoxelShape = ArrayGatePart.cShapes(side)
    override def getOcclusionShape:VoxelShape = ArrayGatePart.oShapes(side)
}

class StackingLatch extends ArrayGatePartTopOnly(GateType.STACKING_LATCH) with TSimpleRSGatePart
{
    override def inputMask(shape:Int) = 4
    override def outputMask(shape:Int) = 1

    override def calcOutput(input:Int):Int = {
        if (signal == 0)
            state>>4
        else
            if ((input&4) == 0) 0 else 1
    }
}
