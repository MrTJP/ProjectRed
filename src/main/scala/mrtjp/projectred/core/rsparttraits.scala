package mrtjp.projectred.core

import codechicken.multipart.api.part.{RedstoneInteractions, TMultiPart}
import codechicken.multipart.api.part.redstone.{IFaceRedstonePart, IRedstonePart}
import mrtjp.projectred.core.IWirePart._
import net.minecraft.block.{Blocks, RedstoneWireBlock}
import net.minecraft.util.Direction
import net.minecraft.util.math.BlockPos

trait TRSAcquisitionsCommons extends TAcquisitionsCommons with IRedstonePart
{
    def calcStraightSignal(r:Int):Int

    def calcInternalSignal(r:Int):Int

    def resolveSignal(part:Any, dir:Int):Int
}

trait TFaceRSAcquisitions extends TRSAcquisitionsCommons with TFaceAcquisitions with IFaceRedstonePart
{
    def calcCornerSignal(r:Int) = resolveSignal(getCorner(r), rotFromCorner(r))

    override def calcStraightSignal(r:Int) = resolveSignal(getStraight(r), rotFromStraight(r))

    override def calcInternalSignal(r:Int) = resolveSignal(getInternal(r), rotFromInternal(r))

    def calcCenterSignal = resolveSignal(getCenter, side)

    def calcStrongSignal(r:Int) = RedstoneInteractions.getPowerTo(this, absoluteDir(r))*17

    def calcWeakSignal(r:Int) =
    {
        val pos = posOfStraight(r)
        if (world.getBlockState(pos).isNormalCube(world, pos))
            world.getRedstonePowerFromNeighbors(pos)*17
        else 0
    }

    def calcMaxSignal(r:Int, strong:Boolean, dustLimit:Boolean):Int =
    {
        var i = calcDustRedwireSignal(r)
        if (i > -1 && dustLimit) return i
        i = calcStrongSignal(r)
        if (i > 0 || strong) return i
        calcWeakSignal(r)
    }

    def calcUndersideSignal =
    {
        val face = Direction.byIndex(side)
        world.getRedstonePower(pos.offset(face), face)*17
    }

    def calcDustRedwireSignal(r:Int) =
    {
        val pos = posOfStraight(r)
        val b = world.getBlockState(pos)
        if (b.getBlock == Blocks.REDSTONE_WIRE)
            Math.max(b.get(RedstoneWireBlock.POWER)-1, 0)
        else -1
    }

    override def getFace = side
}

trait TCenterRSAcquisitions extends TRSAcquisitionsCommons with TCenterAcquisitions
{
    override def calcStraightSignal(s:Int) = resolveSignal(getStraight(s), s^1)

    override def calcInternalSignal(s:Int) = resolveSignal(getInternal(s), s^1)

    def calcStrongSignal(s:Int) = RedstoneInteractions.getPowerTo(this, s)*17

    def calcWeakSignal(s:Int) =
    {
        val pos = this.pos.offset(Direction.byIndex(s))
        if (world.getBlockState(pos).isNormalCube(world, pos))
            world.getRedstonePowerFromNeighbors(pos)*17
        else 0
    }
}

trait TPropagationCommons extends TMultiPart with IWirePart
{
    var propagationMask:Int

    def propagate(prev:TMultiPart, mode:Int)

    def propagateOther(mode:Int){}

    def propagateExternal(to:TMultiPart, at:BlockPos, from:TMultiPart, mode:Int)
    {
        if (to != null) {
            if (to == from) return
            if (propagateTo(to, mode)) return
        }
        WirePropagator.addNeighborChange(at)
    }

    def propagateInternal(to:TMultiPart, from:TMultiPart, mode:Int)
    {
        if (to == from) return
        propagateTo(to, mode)
    }

    def propagateTo(part:TMultiPart, mode:Int) = part match
    {
        case w:IWirePart =>
            WirePropagator.propagateTo(w, this, mode)
            true
        case _ => false
    }
}

trait TFacePropagation extends TPropagationCommons with TFaceConnectable
{
    override var propagationMask = 0xF

    override def propagate(prev:TMultiPart, mode:Int)
    {
        if (mode != FORCED) WirePropagator.addPartChange(this)
        for (r <- 0 until 4) if ((propagationMask&1<<r) != 0) {
            if (maskConnectsInside(r)) propagateInternal(getInternal(r), prev, mode)
            else if (maskConnectsStraight(r)) propagateExternal(getStraight(r), posOfStraight(r), prev, mode)
            else if (maskConnectsCorner(r)) propagateExternal(getCorner(r), posOfCorner(r), prev, mode)
        }

        if (maskConnectsCenter) propagateInternal(getCenter, prev, mode)
        propagateOther(mode)
    }
}

trait TCenterPropagation extends TPropagationCommons with TCenterConnectable
{
    override var propagationMask = 0x3F

    override def propagate(prev:TMultiPart, mode:Int)
    {
        if (mode != FORCED) WirePropagator.addPartChange(this)
        for (s <- 0 until 6) if ((propagationMask&1<<s) != 0) {
            if (maskConnectsIn(s)) propagateInternal(getInternal(s), prev, mode)
            else if (maskConnectsOut(s)) propagateExternal(getStraight(s), posOfStraight(s), prev, mode)
        }
        propagateOther(mode)
    }
}

trait TRSPropagationCommons extends TPropagationCommons
{
    def calculateSignal:Int

    def getSignal:Int
    def setSignal(signal:Int)

    override def updateAndPropagate(prev:TMultiPart, mode:Int)
    {
        if (mode == DROPPING && getSignal == 0) return
        val newSignal = calculateSignal
        if (newSignal < getSignal)
        {
            if (newSignal > 0) WirePropagator.propagateAnalogDrop(this)
            setSignal(0)
            propagate(prev, DROPPING)
        }
        else if (newSignal > getSignal)
        {
            setSignal(newSignal)
            if (mode == DROPPING) propagate(null, RISING)
            else propagate(prev, RISING)
        }
        else if (mode == DROPPING) propagateTo(prev, RISING)
        else if (mode == FORCE) propagate(prev, FORCED)
    }
}

trait TFaceRSPropagation extends TFacePropagation with TRSPropagationCommons

trait TCenterRSPropagation extends TCenterPropagation with TRSPropagationCommons

trait IRedwirePart extends IWirePart with IRedwireEmitter

/**
  * Implemented by parts that emit a full-strength red alloy signal.
  */
trait IRedwireEmitter
{
    /**
      * For face parts, dir is a rotation. For center parts, it is a forge
      * direction.
      *
      * @return Signal strength from 0 to 255.
      */
    def getRedwireSignal(dir:Int):Int
}

trait IInsulatedRedwirePart extends IRedwirePart
{
    def getInsulatedColour:Int
}
