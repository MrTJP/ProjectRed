package mrtjp.projectred.core

import codechicken.lib.data.MCDataInput
import codechicken.lib.vec.Rotation
import codechicken.multipart.api.part.TMultiPart
import codechicken.multipart.block.BlockMultiPart
import codechicken.multipart.util.PartMap
import mrtjp.projectred.api.IConnectable
import net.minecraft.entity.LivingEntity
import net.minecraft.item.ItemStack
import net.minecraft.nbt.CompoundNBT
import net.minecraft.util.Direction
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World

trait TTileAcquisitions extends CoreTile
{
    def getStraightCenter(s:Int):TMultiPart = {
        val pos = posOfInternal.relative(Direction.values()(s))
        val t = BlockMultiPart.getTile(getLevel, pos)
        if (t != null) t.getSlottedPart(6)
        else null
    }

    def getStraight(s:Int, edgeRot:Int):TMultiPart = {
        val pos = posOfStraight(s)
        val t = BlockMultiPart.getTile(getLevel, pos)
        if (t != null) t.getSlottedPart(Rotation.rotateSide(s^1, edgeRot))
        else null
    }

    def getCorner(s:Int, edgeRot:Int):TMultiPart = {
        val pos = posOfCorner(s, edgeRot)
        val t = BlockMultiPart.getTile(getLevel, pos)
        if (t != null) t.getSlottedPart(s^1)
        else null
    }

    def posOfStraight(s:Int):BlockPos = getBlockPos.relative(Direction.values()(s))
    def posOfCorner(s:Int, edgeRot:Int):BlockPos = getBlockPos.relative(Direction.values()(s)).relative(Direction.values()(Rotation.rotateSide(s^1, edgeRot)))
    def posOfInternal:BlockPos = getBlockPos

    def rotFromStraight(s:Int, edgeRot:Int):Int = Rotation.rotationTo(Rotation.rotateSide(s^1, edgeRot), s^1)
    def rotFromCorner(s:Int, edgeRot:Int):Int = Rotation.rotationTo(s^1, Rotation.rotateSide(s^1, edgeRot)^1)

    def notifyStraight(s:Int):Unit = {
        val pos = posOfStraight(s)
        getLevel.neighborChanged(pos, getBlockState.getBlock, posOfInternal)
    }

    def notifyCorner(s:Int, edgeRot:Int):Unit = {
        val pos = posOfCorner(s, edgeRot)
        getLevel.neighborChanged(pos, getBlockState.getBlock, posOfInternal)
    }
}

trait TTileConnectable extends CoreTile with TTileAcquisitions with IConnectable
{
    /**
     * -> full block connection mask
     *
     * 0000 0000 EEEE WWWW SSSS NNNN UUUU DDDD | 00FF FFFF EEEE WWWW SSSS NNNN UUUU DDDD
     *
     *
     * For a full block, you can have a full 6 sides of connection, with 5 on each side.
     *
     * First 8 nibbles, straight connections, of nibble of 1 << r where r is a Rotation.rotationTo(blockSide, edgeSide)
     * D - Down
     * U - Up
     * N - North
     * S - South
     * W - West
     * E - East
     * F - Straight connections to center part or another full block
     *
     * Second 8 nibbles, corner face connections:
     * D - Down
     * U - Up
     * N - North
     * S - South
     * W - West
     * E - East
     */
    var connMap = 0L

    override def connectStraight(part:IConnectable, s:Int, edgeRot:Int):Boolean = {
        if (canConnectPart(part, s, edgeRot)) {
            val old = connMap

            if (edgeRot > -1) connMap |= (0x1<<edgeRot)<<s*4
            else connMap |= 0x1000000<<s*4

            if (old != connMap) onMaskChanged()
            true
        }
        else false
    }

    override def connectCorner(part:IConnectable, s:Int, edgeRot:Int):Boolean = {
        if (canConnectPart(part, s, edgeRot)) {
            val old = connMap
            connMap |= (0x100000000L<<edgeRot)<<s*4
            if (old != connMap) onMaskChanged()
            true
        }
        else false
    }

    override def connectInternal(part:IConnectable, r:Int) = false
    override def canConnectCorner(r:Int) = false

    def canConnectPart(part:IConnectable, s:Int, edgeRot:Int):Boolean
    def onMaskChanged(){}

    def outsideCornerEdgeOpen(s:Int, edgeRot:Int):Boolean = {
        val pos = posOfInternal.relative(Direction.values()(s))
        if (getLevel.isEmptyBlock(pos)) true
        else
        {
            val side1 = s^1
            val side2 = Rotation.rotateSide(s^1, edgeRot)
            val t = BlockMultiPart.getTile(getLevel, pos)
            if (t != null)
                t.getSlottedPart(side1) == null && t.getSlottedPart(side2) == null && t.getSlottedPart(PartMap.edgeBetween(side1, side2)) == null
            else false
        }
    }

    def discoverStraightCenter(s:Int):Boolean = getStraightCenter(s) match  {
        case ic:IConnectable => canConnectPart(ic, s, -1) && ic.connectStraight(this, s^1, -1)
        case _ => discoverStraightOverride(s)
    }

    def discoverStraight(s:Int, edgeRot:Int):Boolean = getStraight(s, edgeRot) match {
        case ic:IConnectable => canConnectPart(ic, s, edgeRot) && ic.connectStraight(this, rotFromStraight(s, edgeRot), -1)
        case _ => false
    }

    def discoverCorner(s:Int, edgeRot:Int):Boolean = getCorner(s, edgeRot) match {
        case ic:IConnectable => canConnectPart(ic, s, edgeRot) && outsideCornerEdgeOpen(s, edgeRot) &&
            ic.canConnectCorner(rotFromCorner(s, edgeRot)) && ic.connectCorner(this, rotFromCorner(s, edgeRot), -1)
        case _ => false
    }

    def discoverStraightOverride(s:Int):Boolean = {//TODO remove to discoverStraightCenterOverride
        val pos = posOfInternal.relative(Direction.values()(s))
        val t = getLevel.getBlockEntity(pos) match {
            case t: IConnectable => t
            case _ => null
        }
        if (t != null && canConnectPart(t, s, -1)) t.connectStraight(this, s^1, -1)
        else false
    }

    def updateExternals():Boolean = {
        var connMap2 = 0L

        for (s <- 0 until 6)
        {
            if (discoverStraightCenter(s)) connMap2 |= 0x1000000<<s

            for (edgeRot <- 0 until 4) if (discoverStraight(s, edgeRot))
                connMap2 |= (0x1<<edgeRot)<<s*4

            for (edgeRot <- 0 until 4) if (discoverCorner(s, edgeRot))
                connMap2 |= (0x100000000L<<edgeRot)<<s*4
        }

        if (connMap != connMap2) {
            connMap = connMap2
            onMaskChanged()
            true
        }
        else false
    }

    def maskConnects(s:Int):Boolean = (connMap&(0xF0000000FL<<(s*4)|0x1000000L<<s)) != 0
    def maskConnectsStraightCenter(s:Int):Boolean = (connMap&0x1000000L<<s) != 0
    def maskConnectsStraight(s:Int, edgeRot:Int):Boolean = (connMap&((1<<edgeRot)<<s*4)) != 0
    def maskConnectsCorner(s:Int, edgeRot:Int):Boolean = (connMap&((0x100000000L<<s*4)<<edgeRot)) != 0
}

trait TConnectableInstTile extends CoreTile with TTileConnectable
{
    def clientNeedsMap = false

    abstract override def saveToNBT(tag:CompoundNBT) = {
        super.saveToNBT(tag)
        tag.putLong("connMap", connMap)
    }

    abstract override def loadFromNBT(tag:CompoundNBT) = {
        super.loadFromNBT(tag)
        connMap = tag.getLong("connMap")
    }

    abstract override def readUpdate(key:Int, in:MCDataInput):Unit = key match
    {
        case 31 => connMap = in.readLong()
        case _ => super.readUpdate(key, in)
    }

    def sendConnUpdate():Unit = if (clientNeedsMap) sendUpdate(31, _.writeLong(connMap))

    abstract override def onMaskChanged():Unit = {
        super.onMaskChanged()
        sendConnUpdate()
    }

    abstract override def onNeighborBlockChanged(neighborPos:BlockPos):Unit = {
        super.onNeighborBlockChanged(neighborPos)
        if (!getLevel.isClientSide) if (updateExternals()) sendConnUpdate()
    }

    abstract override def onBlockPlaced(player:LivingEntity, stack:ItemStack):Unit = {
        super.onBlockPlaced(player, stack)
        if (!getLevel.isClientSide) if (updateExternals()) sendConnUpdate()
    }

    abstract override def onBlockRemoved():Unit = {
        super.onBlockRemoved()

        var cmask = 0
        for (s <- 0 until 6) if (maskConnects(s))
            cmask |= 1<<s
        notifyExternals(cmask)
    }

    def notifyExternals(mask:Int):Unit = {
        var smask = 0

        for (absSide <- 0 until 6) if ((mask&1<<absSide) != 0) {
            val pos = getBlockPos.relative(Direction.values()(absSide))

            getLevel.neighborChanged(pos, getBlockState.getBlock, getBlockPos)
            for (s <- 0 until 6) if (s != (absSide^1) && (smask&1<<s) == 0)
                getLevel.neighborChanged(pos.relative(Direction.values()(s)), getBlockState.getBlock, pos)

            smask |= 1<<absSide
        }
    }
}

trait TPowerTile extends CoreTile with TConnectableInstTile with TCachedPowerConductor
{
    override def idRange:Seq[Int] = 0 until 30

    private def getExternalConductorForFaceConn(s:Int, edgeRot:Int):PowerConductor = {
        if (maskConnectsStraight(s, edgeRot)) {
            getStraight(s, edgeRot) match {
                case tp:IPowerConnectable => tp.conductor(rotFromStraight(s, edgeRot))
                case _ => null
            }
        } else if (maskConnectsCorner(s, edgeRot)) {
            getCorner(s, edgeRot) match {
                case tp:IPowerConnectable => tp.conductor(rotFromCorner(s, edgeRot))
                case _ => null
            }
        } else
            null
    }

    private def getExternalConductorForCenterConn(s:Int):PowerConductor = {
        if (maskConnectsStraightCenter(s)) {
            getStraightCenter(s) match {
                case tp:IPowerConnectable => tp.conductor(s^1)
                case _ => getLevel.getBlockEntity(posOfInternal.relative(Direction.values()(s))) match {
                    case tp:IPowerConnectable => tp.conductor(s^1)
                    case _ => null
                }
            }
        } else
            null
    }

    def getExternalCond(id:Int):PowerConductor = {
        if (0 until 24 contains id) {//side edge conns
            val s = id/4
            val edgeRot = id%4
            getExternalConductorForFaceConn(s, edgeRot)
        } else if (24 until 30 contains id) {//straight face conns
            val s = id-24
            getExternalConductorForCenterConn(s)
        } else
            null
    }

    abstract override def onMaskChanged():Unit = {
        super.onMaskChanged()
        needsCache = true
    }

    abstract override def onNeighborBlockChanged(neighborPos:BlockPos):Unit = {
        super.onNeighborBlockChanged(neighborPos)
        needsCache = true
    }

    override def connWorld:World = getLevel
}
