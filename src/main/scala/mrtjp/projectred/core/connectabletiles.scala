package mrtjp.projectred.core

import codechicken.lib.data.MCDataInput
import codechicken.lib.vec.Rotation
import codechicken.multipart.{BlockMultipart, PartMap}
import mrtjp.core.block.{MTBlockTile, TTileOrient}
import mrtjp.core.world.WorldLib
import mrtjp.projectred.api.IConnectable
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.EnumFacing
import net.minecraft.world.World

trait TTileAcquisitions extends MTBlockTile
{
    def getStraightCenter(s:Int) =
    {
        val pos = posOfInternal.offset(EnumFacing.VALUES(s))
        val t = BlockMultipart.getTile(getWorld, pos)
        if (t != null) t.partMap(6)
        else null
    }

    def getStraight(s:Int, edgeRot:Int) =
    {
        val pos = posOfStraight(s)
        val t = BlockMultipart.getTile(getWorld, pos)
        if (t != null) t.partMap(Rotation.rotateSide(s^1, edgeRot))
        else null
    }

    def getCorner(s:Int, edgeRot:Int) =
    {
        val pos = posOfCorner(s, edgeRot)
        val t = BlockMultipart.getTile(getWorld, pos)
        if (t != null) t.partMap(s^1)
        else null
    }

    def posOfStraight(s:Int) = getPos.offset(EnumFacing.VALUES(s))
    def posOfCorner(s:Int, edgeRot:Int) = getPos.offset(EnumFacing.VALUES(s)).offset(EnumFacing.VALUES(Rotation.rotateSide(s^1, edgeRot)))
    def posOfInternal = getPos

    def rotFromStraight(s:Int, edgeRot:Int) = Rotation.rotationTo(Rotation.rotateSide(s^1, edgeRot), s^1)
    def rotFromCorner(s:Int, edgeRot:Int) = Rotation.rotationTo(s^1, Rotation.rotateSide(s^1, edgeRot)^1)

    def notifyStraight(s:Int)
    {
        val pos = posOfStraight(s)
        getWorld.notifyNeighborsRespectDebug(pos, getBlockType, false)
    }

    def notifyCorner(s:Int, edgeRot:Int)
    {
        val pos = posOfCorner(s, edgeRot)
        getWorld.notifyNeighborsRespectDebug(pos, getBlockType, false)
    }
}

trait TTileConnectable extends MTBlockTile with TTileAcquisitions with IConnectable
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

    override def connectStraight(part:IConnectable, s:Int, edgeRot:Int) =
    {
        if (canConnectPart(part, s, edgeRot))
        {
            val old = connMap

            if (edgeRot > -1) connMap |= (0x1<<edgeRot)<<s*4
            else connMap |= 0x1000000<<s*4

            if (old != connMap) onMaskChanged()
            true
        }
        else false
    }

    override def connectCorner(part:IConnectable, s:Int, edgeRot:Int) =
    {
        if (canConnectPart(part, s, edgeRot))
        {
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

    def outsideCornerEdgeOpen(s:Int, edgeRot:Int) =
    {
        val pos = posOfInternal.offset(EnumFacing.VALUES(s))
        if (getWorld.isAirBlock(pos)) true
        else
        {
            val side1 = s^1
            val side2 = Rotation.rotateSide(s^1, edgeRot)
            val t = BlockMultipart.getTile(getWorld, pos)
            if (t != null)
                t.partMap(side1) == null && t.partMap(side2) == null && t.partMap(PartMap.edgeBetween(side1, side2)) == null
            else false
        }
    }

    def discoverStraightCenter(s:Int) = getStraightCenter(s) match
    {
        case ic:IConnectable => canConnectPart(ic, s, -1) && ic.connectStraight(this, s^1, -1)
        case _ => discoverStraightOverride(s)
    }

    def discoverStraight(s:Int, edgeRot:Int) = getStraight(s, edgeRot) match
    {
        case ic:IConnectable => canConnectPart(ic, s, edgeRot) && ic.connectStraight(this, rotFromStraight(s, edgeRot), -1)
        case _ => false
    }

    def discoverCorner(s:Int, edgeRot:Int) = getCorner(s, edgeRot) match
    {
        case ic:IConnectable => canConnectPart(ic, s, edgeRot) && outsideCornerEdgeOpen(s, edgeRot) &&
            ic.canConnectCorner(rotFromCorner(s, edgeRot)) && ic.connectCorner(this, rotFromCorner(s, edgeRot), -1)
        case _ => false
    }

    def discoverStraightOverride(s:Int) = //TODO remove to discoverStraightCenterOverride
    {
        val pos = posOfInternal.offset(EnumFacing.VALUES(s))
        val t = getWorld.getTileEntity(pos) match {
            case t: TTileConnectable => t
            case _ => null
        }
        if (t != null && canConnectPart(t, s, -1)) t.connectStraight(this, s^1, -1)
        else false
    }

    def updateExternals() =
    {
        var connMap2 = 0L

        for (s <- 0 until 6)
        {
            if (discoverStraightCenter(s)) connMap2 |= 0x1000000<<s

            for (edgeRot <- 0 until 4) if (discoverStraight(s, edgeRot))
                connMap2 |= (0x1<<edgeRot)<<s*4

            for (edgeRot <- 0 until 4) if (discoverCorner(s, edgeRot))
                connMap2 |= (0x100000000L<<edgeRot)<<s*4
        }

        if (connMap != connMap2)
        {
            connMap = connMap2
            onMaskChanged()
            true
        }
        else false
    }

    def maskConnects(s:Int) = (connMap&(0xF0000000FL<<(s*4)|0x1000000L<<s)) != 0
    def maskConnectsStraightCenter(s:Int) = (connMap&0x1000000L<<s) != 0
    def maskConnectsStraight(s:Int, edgeRot:Int) = (connMap&((1<<edgeRot)<<s*4)) != 0
    def maskConnectsCorner(s:Int, edgeRot:Int) = (connMap&((0x100000000L<<s*4)<<edgeRot)) != 0
}

trait TConnectableInstTile extends MTBlockTile with TTileConnectable
{
    def clientNeedsMap = false

    abstract override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setLong("connMap", connMap)
    }

    abstract override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        connMap = tag.getLong("connMap")
    }

    abstract override def read(in:MCDataInput, key:Int) = key match
    {
        case 31 => connMap = in.readLong()
        case _ => super.read(in, key)
    }

    def sendConnUpdate() = if (clientNeedsMap) writeStream(31).writeLong(connMap).sendToChunk(this)

    abstract override def onMaskChanged()
    {
        super.onMaskChanged()
        sendConnUpdate()
    }

    abstract override def onNeighborBlockChange()
    {
        super.onNeighborBlockChange()
        if (!getWorld.isRemote) if (updateExternals()) sendConnUpdate()
    }

    abstract override def onBlockPlaced(side:Int, player:EntityPlayer, stack:ItemStack)
    {
        super.onBlockPlaced(side, player, stack)
        if (!getWorld.isRemote) if (updateExternals()) sendConnUpdate()
    }

    abstract override def onBlockRemoval()
    {
        super.onBlockRemoval()

        var cmask = 0
        for (s <- 0 until 6) if (maskConnects(s))
            cmask |= 1<<s
        notifyExternals(cmask)
    }

    def notifyExternals(mask:Int)
    {
        var smask = 0

        for (absSide <- 0 until 6) if ((mask&1<<absSide) != 0) {
            val pos = getPos.offset(EnumFacing.values()(absSide))

            getWorld.neighborChanged(pos, getBlock, pos)
            for (s <- 0 until 6) if (s != (absSide^1) && (smask&1<<s) == 0)
                getWorld.neighborChanged(pos.offset(EnumFacing.values()(s)), getBlock, pos)

            smask |= 1<<absSide
        }
    }

}

trait TPowerTile extends MTBlockTile with TConnectableInstTile with TCachedPowerConductor
{
    override def idRange = 0 until 30

    def getExternalCond(id:Int):PowerConductor =
    {
        if (0 until 24 contains id) //side edge conns
        {
            val s = id/4
            val edgeRot = id%4
            if (maskConnectsStraight(s, edgeRot)) getStraight(s, edgeRot) match
            {
                case tp:IPowerConnectable => return tp.conductor(rotFromStraight(s, edgeRot))
                case _ =>
            }
            else if (maskConnectsCorner(s, edgeRot)) getCorner(s, edgeRot) match
            {
                case tp:IPowerConnectable => return tp.conductor(rotFromCorner(s, edgeRot))
                case _ =>
            }
        }
        else if (24 until 30 contains id) //straight face conns
        {
            val s = id-24
            if (maskConnectsStraightCenter(s)) getStraightCenter(s) match
            {
                case tp:IPowerConnectable => return tp.conductor(s^1)
                case _ => getWorld.getTileEntity(posOfInternal.offset(EnumFacing.VALUES(s))) match
                {
                    case tp:IPowerConnectable => return tp.conductor(s^1)
                    case _ =>
                }
            }
        }
        null
    }

    abstract override def onMaskChanged()
    {
        super.onMaskChanged()
        needsCache = true
    }

    override def connWorld: World = getWorld
}

