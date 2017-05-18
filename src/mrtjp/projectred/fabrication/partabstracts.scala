/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.vec.{Rotation, Vector3}
import mrtjp.core.vec.Point

trait TICTileOrient extends ICTile
{
    var orientation:Byte = 0

    def rotation = orientation&0x3

    def setRotation(r:Int)
    {
        orientation = (orientation&0xFC|r).toByte
    }

    def rotationT = Rotation.quarterRotations(rotation).at(new Vector3(0.5, 0, 0.5))

    // internal r from absRot
    def toInternal(absRot:Int) = (absRot+4-rotation)%4
    // absRot from internal r
    def toAbsolute(r:Int) = (r+rotation)%4

    def toInternalMask(mask:Int) = TICTileOrient.shiftMask(mask, toInternal(0))
    def toAbsoluteMask(mask:Int) = TICTileOrient.shiftMask(mask, toAbsolute(0))
}

object TICTileOrient
{
    def shiftMask(mask:Int, r:Int) = (mask& ~0xF)|(mask<<r|mask>>4-r)&0xF
    def flipMaskZ(mask:Int) = mask&5|mask<<2&8|mask>>2&2
}

trait TICTileAcquisitions extends ICTile
{
    def getStraight(r:Int) = tileMap.getTile(posOfStraight(r))
    def posOfStraight(r:Int) = Point(x, y).offset(r)
    def rotFromStraight(r:Int) = (r+2)%4

    def notifyToDir(r:Int){editor.notifyNeighbor(posOfStraight(r))}
    def notify(mask:Int){editor.notifyNeighbors(x, y, mask)}
}

trait TConnectableICTile extends ICTile with TICTileAcquisitions
{
    var connMap:Byte = 0

    def maskConnects(r:Int) = (connMap&1<<r) != 0

    def discover(r:Int) = getStraight(r) match
    {
        case c:TConnectableICTile => canConnectTile(c, r) && c.connect(this, rotFromStraight(r))
        case c => discoverOverride(r, c)
    }

    def discoverOverride(r:Int, part:ICTile) = false

    def connect(tile:ICTile, r:Int) =
    {
        if (canConnectTile(tile, r)) {
            val oldConn = connMap
            connMap = (connMap|1<<r).toByte
            if (oldConn != connMap) onMaskChanged()
            true
        }
        else false
    }

    def updateConns() =
    {
        var newConn = 0
        for (r <- 0 until 4) if (discover(r)) newConn |= 1<<r
        if (newConn != connMap) {
            connMap = newConn.toByte
            onMaskChanged()
            true
        }
        else false
    }

    def canConnectTile(tile:ICTile, r:Int):Boolean

    def onMaskChanged(){}
}