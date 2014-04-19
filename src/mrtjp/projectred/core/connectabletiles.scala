package mrtjp.projectred.core

import codechicken.lib.data.MCDataInput
import codechicken.lib.vec.{BlockCoord, Rotation}
import mrtjp.projectred.api.IConnectable
import mrtjp.projectred.core.blockutil.TileMulti
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.tileentity.TileEntity
import mrtjp.projectred.core.libmc.{MultiTileTile, BasicUtils}
import net.minecraft.block.Block

trait TConnectableTile extends TileEntity with IConnectable
{
    var connMap = 0L

    def connectStraight(part:IConnectable, absDir:Int, edgeRot:Int):Boolean =
    {
        if (canConnect(part))
        {
            val old = connMap

            if (edgeRot > -1) connMap |= (0x1<<edgeRot)<<absDir
            else connMap |= 0x1000000<<absDir

            if (old != connMap) sendConnUpdate()
            return true
        }

        false
    }

    def connectCorner(part:IConnectable, absDir:Int, edgeRot:Int):Boolean =
    {
        if (canConnect(part))
        {
            val old = connMap
            connMap |= (0x100000000L<<edgeRot)<<absDir

            if (old != connMap) sendConnUpdate()
            return true
        }
        false
    }

    def connectInternal(part:IConnectable, r:Int) = false

    def canConnectCorner(r:Int) = false

    def updateExternals():Boolean =
    {
        var connMap2 = 0L

        for (absDir <- 0 until 6)
        {
            if (tryConnectStraight(absDir, -1)) connMap2 |= 0x1000000<<absDir

            for (edgeRot <- 0 until 4) if (tryConnectStraight(absDir, edgeRot))
                connMap2 |= (0x1<<edgeRot)<<absDir

            for (edgeRot <- 0 until 4) if (tryConnectCorner(absDir, edgeRot))
                connMap2 |= (0x100000000L<<edgeRot)<<absDir
        }

        if (connMap != connMap2)
        {
            connMap = connMap2
            return true
        }
        false
    }

    def tryConnectStraight(absDir:Int, edgeRot:Int):Boolean =
    {
        val tp = BlockConnLib.getStraight(worldObj, absDir, edgeRot, new BlockCoord(this))

        if (tp != null)
        {
            if (edgeRot > -1)
            {
                val sideOfConnPart = Rotation.rotateSide(absDir^1, edgeRot)
                val rotToThis = Rotation.rotationTo(sideOfConnPart, absDir^1)

                if (tp.isInstanceOf[IConnectable] && canConnect(tp.asInstanceOf[IConnectable]))
                    return tp.asInstanceOf[IConnectable].connectStraight(this, rotToThis, -1)

                return false
            }
            else
            {
                if (tp.isInstanceOf[IConnectable] && canConnect(tp.asInstanceOf[IConnectable]))
                    return tp.asInstanceOf[IConnectable].connectStraight(this, absDir^1, -1)
            }
        }
        tryConnectStraightOverride(absDir)
    }

    def tryConnectStraightOverride(absDir:Int):Boolean =
    {
        val pos = new BlockCoord(this).offset(absDir)
        val t = BasicUtils.getTileEntity(worldObj, pos, classOf[TConnectableTile])
        if (t != null && canConnect(t)) return t.connectStraight(this, absDir^1, -1)

        false
    }

    def tryConnectCorner(absDir:Int, edgeRot:Int):Boolean =
    {
        val tp = BlockConnLib.getCorner(worldObj, absDir, edgeRot, new BlockCoord(this))
        if (tp != null && tp.isInstanceOf[IConnectable] && canConnect(tp.asInstanceOf[IConnectable]))
        {
            val sideTo = Rotation.rotateSide(absDir^1, edgeRot)
            val sideOfConnPart = absDir^1
            val rotToThis = Rotation.rotationTo(sideOfConnPart, sideTo^1)

            if (tp.asInstanceOf[IConnectable].canConnectCorner(rotToThis))
                return tp.asInstanceOf[IConnectable].connectCorner(this, rotToThis, -1)
        }
        false
    }

    def canConnect(part:IConnectable):Boolean

    def sendConnUpdate()

    def rebuildConns() = if (updateExternals()) sendConnUpdate()
}

trait TConnectableTileMulti extends MultiTileTile with TConnectableTile
{
    def clientNeedsMap = false

    def sendConnUpdate() = if (clientNeedsMap) writeStreamSend(writeStream(31).writeLong(connMap))

    abstract override def read(in:MCDataInput, switchkey:Int) = switchkey match
    {
        case 31 => connMap = in.readLong()
        case _ => super.read(in, switchkey)
    }

    abstract override def onNeighborChange(b:Block)
    {
        super.onNeighborChange(b)
        if (!worldObj.isRemote) rebuildConns()
    }

    abstract override def onBlockPlaced(side:Int)
    {
        super.onBlockPlaced(side)
        if (!worldObj.isRemote) rebuildConns()
    }

    abstract override def onBlockRemoval()
    {
        super.onBlockRemoval()
        BasicUtils.updateIndirectNeighbors(worldObj, xCoord, yCoord, zCoord, getBlock)
    }
}