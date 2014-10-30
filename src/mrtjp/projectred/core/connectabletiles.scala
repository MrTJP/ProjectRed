package mrtjp.projectred.core

import codechicken.lib.data.MCDataInput
import codechicken.lib.vec.{BlockCoord, Rotation, Vector3}
import codechicken.multipart.PartMap
import mrtjp.core.block.InstancedBlockTile
import mrtjp.core.world.WorldLib
import mrtjp.projectred.api.IConnectable
import mrtjp.projectred.core.libmc.PRLib
import net.minecraft.block.Block
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack

trait TTileAcquisitions extends InstancedBlockTile
{
    def getStraightCenter(s:Int) =
    {
        val pos = posOfInternal.offset(s)
        val t = PRLib.getMultipartTile(world, pos)
        if (t != null) t.partMap(6)
        else null
    }

    def getStraight(s:Int, edgeRot:Int) =
    {
        val pos = posOfStraight(s)
        val t = PRLib.getMultipartTile(world, pos)
        if (t != null) t.partMap(Rotation.rotateSide(s^1, edgeRot))
        else null
    }

    def getCorner(s:Int, edgeRot:Int) =
    {
        val pos = posOfCorner(s, edgeRot)
        val t = PRLib.getMultipartTile(world, pos)
        if (t != null) t.partMap(s^1)
        else null
    }

    def posOfStraight(s:Int) = new BlockCoord(this).offset(s)
    def posOfCorner(s:Int, edgeRot:Int) = new BlockCoord(this).offset(s).offset(Rotation.rotateSide(s^1, edgeRot))
    def posOfInternal = new BlockCoord(this)

    def rotFromStraight(s:Int, edgeRot:Int) = Rotation.rotationTo(Rotation.rotateSide(s^1, edgeRot), s^1)
    def rotFromCorner(s:Int, edgeRot:Int) = Rotation.rotationTo(s^1, Rotation.rotateSide(s^1, edgeRot)^1)

    def notifyStraight(s:Int)
    {
        val pos = posOfStraight(s)
        world.notifyBlockOfNeighborChange(pos.x, pos.y, pos.z, getBlockType)
    }

    def notifyCorner(s:Int, edgeRot:Int)
    {
        val pos = posOfCorner(s, edgeRot)
        world.notifyBlockOfNeighborChange(pos.x, pos.y, pos.z, getBlockType)
    }
}

trait TTileConnectable extends InstancedBlockTile with TTileAcquisitions with IConnectable
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
        if (canConnect(part))
        {
            val old = connMap

            if (edgeRot > -1) connMap |= (0x1<<edgeRot)<<s
            else connMap |= 0x1000000<<s

            if (old != connMap) onMaskChanged()
            true
        }
        false
    }

    override def connectCorner(part:IConnectable, s:Int, edgeRot:Int) =
    {
        if (canConnect(part))
        {
            val old = connMap
            connMap |= (0x100000000L<<edgeRot)<<s
            if (old != connMap) onMaskChanged()
            true
        }
        false
    }

    override def connectInternal(part:IConnectable, r:Int) = false
    override def canConnectCorner(r:Int) = false

    def canConnect(part:IConnectable):Boolean
    def onMaskChanged(){}

    def outsideCornerEdgeOpen(s:Int, edgeRot:Int) =
    {
        val pos = posOfInternal.offset(s)
        if (world.isAirBlock(pos.x, pos.y, pos.z)) true
        else
        {
            val side1 = s^1
            val side2 = Rotation.rotateSide(s^1, edgeRot)
            val t = PRLib.getMultipartTile(world, pos)
            if (t != null)
                t.partMap(side1) == null && t.partMap(side2) == null && t.partMap(PartMap.edgeBetween(side1, side2)) == null
            else false
        }
    }

    def discoverStraightCenter(s:Int) = getStraightCenter(s) match
    {
        case ic:IConnectable => canConnect(ic) && ic.connectStraight(this, s^1, -1)
        case _ => false
    }

    def discoverStraight(s:Int, edgeRot:Int) = getStraight(s, edgeRot) match
    {
        case ic:IConnectable => canConnect(ic) && ic.connectStraight(this, rotFromStraight(s, edgeRot), -1)
        case _ => discoverStraightOverride(s)
    }

    def discoverCorner(s:Int, edgeRot:Int) = getCorner(s, edgeRot) match
    {
        case ic:IConnectable => canConnect(ic) && outsideCornerEdgeOpen(s, edgeRot) &&
            ic.connectCorner(this, rotFromCorner(s, edgeRot), -1)
        case _ => false
    }

    def discoverStraightOverride(s:Int) =
    {
        val pos = posOfInternal.offset(s)
        val t = WorldLib.getTileEntity(getWorldObj, pos, classOf[TTileConnectable])
        if (t != null && canConnect(t)) t.connectStraight(this, s^1, -1)
        else false
    }

    def updateExternals() =
    {
        var connMap2 = 0L

        for (s <- 0 until 6)
        {
            if (discoverStraightCenter(s)) connMap2 |= 0x1000000<<s

            for (edgeRot <- 0 until 4) if (discoverStraight(s, edgeRot))
                connMap2 |= (0x1<<edgeRot)<<s

            for (edgeRot <- 0 until 4) if (discoverCorner(s, edgeRot))
                connMap2 |= (0x100000000L<<edgeRot)<<s
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
    def maskConnectsStraight(s:Int, edgeRot:Int) = (connMap&((1<<s*4)<<edgeRot)) != 0
    def maskConnectsCorner(s:Int, edgeRot:Int) = (connMap&((0x100000000L<<s*4)<<edgeRot)) != 0
}

trait TConnectableInstTile extends InstancedBlockTile with TTileConnectable
{
    def clientNeedsMap = false

    def sendConnUpdate() = if (clientNeedsMap) writeStream(31).writeLong(connMap).sendToChunk()

    override def onMaskChanged(){sendConnUpdate()}

    abstract override def read(in:MCDataInput, switchkey:Int) = switchkey match
    {
        case 31 => connMap = in.readLong()
        case _ => super.read(in, switchkey)
    }

    abstract override def onNeighborChange(b:Block)
    {
        super.onNeighborChange(b)
        if (!getWorldObj.isRemote) if (updateExternals()) sendConnUpdate()
    }

    abstract override def onBlockPlaced(side:Int, meta:Int, player:EntityPlayer, stack:ItemStack, hit:Vector3)
    {
        super.onBlockPlaced(side, meta, player, stack, hit)
        if (!getWorldObj.isRemote) if (updateExternals()) sendConnUpdate()
    }

    abstract override def onBlockRemoval()
    {
        super.onBlockRemoval()
        WorldLib.bulkBlockUpdate(getWorldObj, xCoord, yCoord, zCoord, getBlock)
    }
}