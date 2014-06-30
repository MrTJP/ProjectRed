package mrtjp.projectred.core

import codechicken.lib.vec.{BlockCoord, Rotation}
import codechicken.multipart._
import mrtjp.projectred.api.IConnectable
import mrtjp.projectred.core.libmc.PRLib

trait TAcquisitionsCommons extends TMultiPart
{
    def getStraight(r:Int):TMultiPart
    def getInternal(r:Int):TMultiPart

    def posOfStraight(r:Int):BlockCoord
    def posOfInternal = new BlockCoord(tile)

    def notifyStraight(dir:Int)
    {
        val pos = posOfStraight(dir)
        world.notifyBlockOfNeighborChange(pos.x, pos.y, pos.z, tile.getBlockType)
    }

    def notifyInternal(dir:Int)
    {
        tile.notifyPartChange(getInternal(dir))
    }
}

trait TFaceAcquisitions extends TAcquisitionsCommons with TFaceOrient
{
    def getCorner(r:Int) =
    {
        val absDir = absoluteDir(r)
        val pos = new BlockCoord(tile).offset(absDir).offset(side)

        val t = PRLib.getMultipartTile(world, pos)
        if (t != null) t.partMap(absDir^1)
        else null
    }

    def getStraight(r:Int) =
    {
        val pos = new BlockCoord(tile).offset(absoluteDir(r))

        val t = PRLib.getMultipartTile(world, pos)
        if (t != null) t.partMap(side)
        else null
    }

    def getInternal(r:Int) = tile.partMap(absoluteDir(r))

    def getCenter = tile.partMap(6)

    def posOfCorner(r:Int) = new BlockCoord(tile).offset(absoluteDir(r)).offset(side)
    def posOfStraight(r:Int) = new BlockCoord(tile).offset(absoluteDir(r))

    def rotFromCorner(r:Int) = Rotation.rotationTo(absoluteDir(r)^1, side^1)
    def rotFromStraight(r:Int) = (r+2)%4
    def rotFromInternal(r:Int) = Rotation.rotationTo(absoluteDir(r), side)

    def notifyCorner(r:Int)
    {
        val pos = posOfCorner(r)
        world.notifyBlockOfNeighborChange(pos.x, pos.y, pos.z, tile.getBlockType)
    }
}

trait TCenterAcquisitions extends TAcquisitionsCommons with TCenterOrient
{
    def getStraight(s:Int) =
    {
        val pos = posOfInternal.offset(s)
        val t = PRLib.getMultipartTile(world, pos)
        if (t != null) t.partMap(6)
        else null
    }

    def getInternal(s:Int) = tile.partMap(s)

    def posOfStraight(s:Int) = new BlockCoord(tile).offset(s)
}

trait TConnectableCommons extends TMultiPart with IConnectable
{
    var connMap:Int

    /**
     * Sets of defs that are common in all subtypes.
     * dir is rotation for face, absDir for center implementations
     */

    /**
     * Should always return true if this is a logic part for example, where
     * mask is always open, because edges are are all the way on the side,
     * so strips cant block conns.
     */
    def maskOpen(dir:Int):Boolean
    def maskConnects(dir:Int):Boolean

    /**
     * Used to determine which conns can be made outside. Implementations include
     * 1) for parts that can make internal conns, check if strips, etc. is blocking.
     * 2) for parts that cannot, always true (i.e. logic tiles)
     * @param dir rot, or absDir (face vs center parts)
     * @return true if a connection is possible
     */
    def discoverOpen(dir:Int):Boolean

    def discoverStraight(dir:Int):Boolean
    def discoverInternal(dir:Int):Boolean

    def discoverStraightOverride(absDir:Int):Boolean
    def discoverInternalOverride(p:TMultiPart, dir:Int):Boolean

    def canConnectPart(part:IConnectable, dir:Int):Boolean

    /**
     * Recalculates connections that can be made to other parts outside of this
     * space
     *
     * @return true if external connections should be recalculated
     */
    def updateOpenConns():Boolean

    /**
     * Recalculates connections to blocks outside this sapce
     *
     * @return true if a new connection was added or one was removed
     */
    def updateExternalConns():Boolean

    /**
     * Recalculates connections to other parts within this space
     *
     * @return true if a new connection was added or one was removed
     */
    def updateInternalConns():Boolean

    /**
     * Start update chain starting from an internal change outward
     * @return true if a new connection was added or one was removed
     */
    def updateOutward() =
    {
        var changed = updateInternalConns()
        if (updateOpenConns()) changed |= updateExternalConns()
        changed
    }

    /**
     * Start update chain starting from an external change inward
     * @return true if a new connection was added or one was removed
     */
    def updateInward() =
    {
        updateOpenConns()
        var changed = updateInternalConns()
        changed |= updateExternalConns()
        changed
    }

    def notifyAllExternals()

    def onMaskChanged() {}
}

trait TFaceConnectable extends TConnectableCommons with TFaceAcquisitions
{
    /**
     * Split into 4 nybbles: (from lowest)
     * 0 = Corner connections (this wire should connect around a corner to something external)
     * 1 = Straight connections (this wire should connect to something external)
     * 2 = Internal connections (this wire should connect to something internal)
     * 3 = Internal open connections (this wire is not blocked by a cover/edge part and *could* connect through side)
     * bit 16 = connection to the centerpart
     * 5 = Render corner connections. Like corner connections but set to low if the other wire part is smaller than this (they render to us not us to them)
     */
    var connMap = 0

    override def connectCorner(part:IConnectable, r:Int, edgeRot:Int) =
    {
        if (canConnectPart(part, r) && maskOpen(r))
        {
            val oldConn = connMap
            connMap |= 1<<r
            if (setRenderFlag(part)) connMap |= 0x100000<<r
            if (oldConn != connMap) onMaskChanged()
            true
        }
        else false
    }

    override def connectStraight(part:IConnectable, r:Int, edgeRot:Int) =
    {
        if (canConnectPart(part, r) && maskOpen(r))
        {
            val oldConn = connMap
            connMap |= 0x10<<r
            if (oldConn != connMap) onMaskChanged()
            true
        }
        else false
    }

    override def connectInternal(part:IConnectable, r:Int) =
    {
        if (canConnectPart(part, r))
        {
            val oldConn = connMap
            connMap |= 0x100<<r
            if (oldConn != connMap) onMaskChanged()
            true
        }
        else false
    }

    /**
     * If this is a wire, should return true if this wire is smaller
     * than that wire. This is used for corner rendering. (This renders
     * to that, not that to this). Always false if this is not a wire.
     * @param part The part to connect to
     * @return true if this should render instead of that
     */
    def setRenderFlag(part:IConnectable):Boolean

    def maskOpen(r:Int) = (connMap&0x1000<<r) != 0
    def maskConnects(r:Int) = (connMap&0x111<<r) != 0
    def maskConnectsCorner(r:Int) = (connMap&1<<r) != 0
    def maskConnectsStraight(r:Int) = (connMap&0x10<<r) != 0
    def maskConnectsInside(r:Int) = (connMap&0x100<<r) != 0
    def maskConnectsCenter = (connMap&0x10000) != 0

    def outsideCornerEdgeOpen(r:Int) =
    {
        val absDir = absoluteDir(r)
        val pos = new BlockCoord(tile).offset(absDir)
        if (world.isAirBlock(pos.x, pos.y, pos.z)) true
        else
        {
            val side1 = absDir^1
            val side2 = side
            val t = PRLib.getMultipartTile(world, pos)
            if (t != null)
                t.partMap(side1) == null && t.partMap(side2) == null && t.partMap(PartMap.edgeBetween(side1, side2)) == null
            else false
        }
    }

    def discoverCorner(r:Int) =
    {
        if (outsideCornerEdgeOpen(r)) getCorner(r) match
        {
            case c:IConnectable =>
                if ((c.canConnectCorner(rotFromCorner(r)) || canConnectCorner(r)) &&
                    canConnectPart(c, r) && c.connectCorner(this, rotFromCorner(r), -1))
                    if (setRenderFlag(c)) 2 else 1
                else 0
            case _ => 0
        }
        else if (discoverCornerOverride(absoluteDir(r))) 2 else 0
    }

    def discoverStraight(r:Int) = getStraight(r) match
    {
        case c:IConnectable => canConnectPart(c, r) && c.connectStraight(this, rotFromStraight(r), -1)
        case _ => discoverStraightOverride(absoluteDir(r))
    }

    def discoverInternal(r:Int) =
    {
        if (tile.partMap(PartMap.edgeBetween(absoluteDir(r), side)) == null) getInternal(r) match
        {
            case c:IConnectable => canConnectPart(c, r) && c.connectInternal(this, rotFromInternal(r))
            case p => discoverInternalOverride(p, r)
        }
        else false
    }

    def discoverCenter = getCenter match
    {
        case c:IConnectable => c.connectInternal(this, side)
        case _ => false
    }

    def discoverCornerOverride(absDir:Int) = false
    def discoverStraightOverride(absDir:Int) = false
    def discoverInternalOverride(p:TMultiPart, r:Int) = false

    /**
     * Recalculates connections that can be made to other parts outside of this
     * space
     *
     * @return true if external connections should be recalculated
     */
    def updateOpenConns() =
    {
        var newConn = 0
        for (r <- 0 until 4) if (discoverOpen(r)) newConn |= 0x1000<<r
        if (newConn != (connMap&0xF000))
        {
            connMap = connMap& ~0xF000|newConn
            true
        }
        else false
    }

    /**
     * Recalculates connections to blocks outside this sapce
     *
     * @return true if a new connection was added or one was removed
     */
    def updateExternalConns() =
    {
        var newConn = 0
        for (r <- 0 until 4) if (maskOpen(r))
        {
            if (discoverStraight(r)) newConn |= 0x10<<r
            else
            {
                val cnrMode = discoverCorner(r)
                if (cnrMode != 0)
                {
                    newConn |= 1<<r
                    if (cnrMode == 2) newConn |= 0x100000<<r
                }
            }
        }
        if (newConn != (connMap&0xF000FF))
        {
            val diff = connMap^newConn //corners need to be notified, because normal block updates wont touch them
            connMap = connMap& ~0xF000FF|newConn
            for (r <- 0 until 4) if ((diff&1<<r)!=0) notifyCorner(r)
            true
        }
        else false
    }

    /**
     * Recalculates connections to other parts within this space
     *
     * @return true if a new connection was added or one was removed
     */
    def updateInternalConns() =
    {
        var newConn = 0
        for (r <- 0 until 4) if (discoverInternal(r)) newConn |= 0x100<<r
        if (shouldDiscoverCenter && discoverCenter) newConn |= 0x10000
        if (newConn != (connMap&0x10F00))
        {
            connMap = connMap& ~0x10F00|newConn
            true
        }
        else false
    }
    def shouldDiscoverCenter = true

    def notifyAllExternals()
    {
        for (r <- 0 until 4) if (maskConnects(r))
            if (maskConnectsCorner(r)) notifyCorner(r)
            else if (maskConnectsStraight(r)) notifyStraight(r)
    }
}

trait TCenterConnectable extends TConnectableCommons with TCenterAcquisitions
{
    /**
     * Split into 3 sections: (from lowest)
     * 0x3F = Straight conns to outside
     * 0xFC0 = Internal conns to face (mixed with above for client, see clientConnMap)
     * 0x3F000 = External open connections (this wire is not blocked by a cover part and *could* connect through side)
     */
    var connMap = 0

    override def canConnectCorner(r:Int) = false

    override def connectStraight(part:IConnectable, s:Int, edgeSide:Int) =
    {
        if (canConnectPart(part, s) && maskOpen(s))
        {
            val oldConn = connMap
            connMap |= 1<<s
            if (oldConn != connMap) onMaskChanged()
            true
        }
        else false
    }

    override def connectInternal(part:IConnectable, s:Int):Boolean =
    {
        if (canConnectPart(part, s))
        {
            val oldConn = connMap
            connMap |= 1<<s+6
            if (oldConn != connMap) onMaskChanged()
            true
        }
        else false
    }

    override def connectCorner(wire:IConnectable, r:Int, edgeSide:Int) = false

    def canConnectPart(part:IConnectable, s:Int):Boolean

    def maskOpen(s:Int) = (connMap&0x1000<<s) != 0
    def maskConnects(s:Int) = (connMap&0x41<<s) != 0
    def maskConnectsOut(s:Int) = (connMap&1<<s) != 0
    def maskConnectsIn(s:Int) = (connMap&1<<s+6) != 0

    def discoverStraight(s:Int) = getStraight(s) match
    {
        case c:IConnectable => canConnectPart(c, s) && c.connectStraight(this, s^1, -1)
        case _ => discoverStraightOverride(s)
    }

    def discoverInternal(s:Int) = getInternal(s) match
    {
        case c:IConnectable => canConnectPart(c, s) && c.connectInternal(this, -1)
        case p => discoverInternalOverride(p, s)
    }

    def discoverStraightOverride(s:Int) = false
    def discoverInternalOverride(p:TMultiPart, s:Int) = false

    def updateOpenConns() =
    {
        var newConn = 0
        for (s <- 0 until 6) if (discoverOpen(s)) newConn |= 1<<s+12
        if (newConn != (connMap&0x3F000))
        {
            connMap = connMap& ~0x3F000|newConn
            true
        }
        else false
    }

    def updateExternalConns() =
    {
        var newConn = 0
        for (s <- 0 until 6) if (maskOpen(s)) if (discoverStraight(s)) newConn |= 1<<s

        if (newConn != (connMap&0x3f))
        {
            connMap = connMap& ~0x3F|newConn
            true
        }
        else false
    }

    def updateInternalConns() =
    {
        var newConn = 0
        for (s <- 0 until 6) if (discoverInternal(s)) newConn |= 1<<s+6

        if (newConn != (connMap&0xFC0))
        {
            connMap = connMap& ~0xFC0|newConn
            true
        }
        else false
    }

    def notifyAllExternals()
    {
        for (s <- 0 until 6) if (maskConnectsOut(s)) notifyStraight(s)
    }
}