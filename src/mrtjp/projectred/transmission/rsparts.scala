package mrtjp.projectred.transmission

import codechicken.lib.vec.BlockCoord
import codechicken.multipart.{IFaceRedstonePart, IRedstonePart, RedstoneInteractions, TMultiPart}
import mrtjp.core.world.WorldLib
import mrtjp.projectred.core._
import mrtjp.projectred.transmission.IWirePart._
import net.minecraft.init.Blocks

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
        val pos = new BlockCoord(tile).offset(absoluteDir(r))
        if (world.isBlockNormalCubeDefault(pos.x, pos.y, pos.z, false)) world.getBlockPowerInput(pos.x, pos.y, pos.z)*17
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
        val pos = new BlockCoord(tile).offset(side)
        world.getIndirectPowerLevelTo(pos.x, pos.y, pos.z, side)*17
    }

    def calcDustRedwireSignal(r:Int) =
    {
        val pos = posOfStraight(r)
        val b = world.getBlock(pos.x, pos.y, pos.z)
        if (b == Blocks.redstone_wire) Math.max(world.getBlockMetadata(pos.x, pos.y, pos.z)-1, 0)
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
        val pos = new BlockCoord(tile).offset(s)
        if (world.isBlockNormalCubeDefault(pos.x, pos.y, pos.z, false)) world.getBlockPowerInput(pos.x, pos.y, pos.z)*17
        else 0
    }
}

trait TPropagationCommons extends TMultiPart with IWirePart
{
    var propagationMask:Int

    def propagate(prev:TMultiPart, mode:Int)

    def propagateOther(mode:Int){}

    def propagateExternal(to:TMultiPart, at:BlockCoord, from:TMultiPart, mode:Int)
    {
        if (to != null)
        {
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
        for (r <- 0 until 4) if ((propagationMask&1<<r) != 0)
        {
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
        for (s <- 0 until 6) if ((propagationMask&1<<s) != 0)
        {
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