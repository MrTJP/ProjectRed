package mrtjp.projectred.transmission

import codechicken.lib.vec.BlockCoord
import codechicken.multipart.{IRedstonePart, IFaceRedstonePart, TMultiPart, RedstoneInteractions}
import mrtjp.projectred.transportation.RouteFX

trait TRSAcquisitionsCommons extends TAcquisitionsCommons with IRedstonePart
{
    def calcStraightSignal(r:Int):Int

    def calcInternalSignal(r:Int):Int

    def resolveSignal(part:TMultiPart, dir:Int):Int
}

trait TFaceRSAcquisitions extends TRSAcquisitionsCommons with TFaceAcquisitions with IFaceRedstonePart
{
    def calcCornerSignal(r:Int) = resolveSignal(getCorner(r), rotFromCorner(r))

    def calcStraightSignal(r:Int) = resolveSignal(getStraight(r), rotFromStraight(r))

    def calcInternalSignal(r:Int) = resolveSignal(getInternal(r), rotFromInternal(r))

    def calcCenterSignal = resolveSignal(getCenter, side)

    def calcStrongSignal(r:Int) = RedstoneInteractions.getPowerTo(this, absoluteDir(r))*17

    def calcWeakSignal(r:Int) =
    {
        val pos = new BlockCoord(tile).offset(absoluteDir(r))
        if (world.isBlockNormalCube(pos.x, pos.y, pos.z)) world.getBlockPowerInput(pos.x, pos.y, pos.z)*17
        else 0
    }

    def calcUndersideSignal =
    {
        val pos = new BlockCoord(tile).offset(side)
        world.getIndirectPowerLevelTo(pos.x, pos.y, pos.z, side)*17
    }

    override def getFace = side
}

trait TCenterRSAcquisitions extends TRSAcquisitionsCommons with TCenterAcquisitions
{
    def calcStraightSignal(s:Int) = resolveSignal(getStraight(s), s^1)

    def calcInternalSignal(s:Int) = resolveSignal(getInternal(s), s^1)

    def calcStrongSignal(s:Int) = RedstoneInteractions.getPowerTo(this, s)*17

    def calcWeakSignal(s:Int) =
    {
        val pos = new BlockCoord(tile).offset(s)
        if (world.isBlockNormalCube(pos.x, pos.y, pos.z)) world.getBlockPowerInput(pos.x, pos.y, pos.z)*17
        else 0
    }
}

trait TPropagationAcquisitions extends TMultiPart with IWirePart
{
    def propagateExternal(to:TMultiPart, at:BlockCoord, from:TMultiPart, mode:Int)
    {
        if (to != null)
        {
            if (to == from) return
            RouteFX.spawnType1(0, 16, at, to.world)
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