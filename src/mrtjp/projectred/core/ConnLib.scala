package mrtjp.projectred.core

import codechicken.lib.vec.{Rotation, BlockCoord}
import codechicken.multipart.PartMap
import mrtjp.projectred.api.IConnectable
import net.minecraft.world.World
import mrtjp.projectred.core.libmc.PRLib

object WireConnLib
{
   /**
     * -> Standard wire connection mask
     *
     * V000 0000 RRRR 000B NNNN IIII SSSS CCCC
     *
     *
     * C - corner connections at (1 << r) where r is a rotation about the face
     * S - straight connections at (1 << r) where r is a rotation about the face
     * I - internal connections at (1 << r) where r is a rotation about the face
     * N - open connections at (1 << r) where r is a rotation about the face
     * B - connection flagged for connection to center part
     * R - render connection flags if client should render them
     * V - Unused bit flagged for RP2 to PR world converter
     */

    def getCorner(world:World, side:Int, r:Int, from:BlockCoord):IConnectable =
    {
        val absDir = Rotation.rotateSide(side, r)
        val pos = from.offset(absDir)

        if (!canConnectThroughCorner(world, pos, absDir^1, side)) return null

        pos.offset(side)

        val t = PRLib.getMultipartTile(world, pos)
        if (t != null)
        {
            val tp = t.partMap(absDir^1)
            if (tp != null && tp.isInstanceOf[IConnectable]) return tp.asInstanceOf[IConnectable]
        }
        null
    }

    def canConnectThroughCorner(world:World, pos:BlockCoord, side1:Int, side2:Int):Boolean =
    {
        if (world.isAirBlock(pos.x, pos.y, pos.z)) return true
        val t = PRLib.getMultipartTile(world, pos)

        if (t != null) return t.partMap(side1) == null && t.partMap(side2) == null && t.partMap(PartMap.edgeBetween(side1, side2)) == null

        false
    }

    def getStraight(world:World, side:Int, r:Int, from:BlockCoord):IConnectable =
    {
        val absDir = Rotation.rotateSide(side, r)
        val pos = from.offset(absDir)
        val t = PRLib.getMultipartTile(world, pos)

        if (t != null)
        {
            val tp = t.partMap(side)
            if (tp != null && tp.isInstanceOf[IConnectable]) return tp.asInstanceOf[IConnectable]
        }
        null
    }

    def getInsideFace(world:World, side:Int, r:Int, from:BlockCoord):IConnectable =
    {
        val absDir = Rotation.rotateSide(side, r)
        val t = PRLib.getMultipartTile(world, from)

        if (t != null)
        {
            if (t.partMap(PartMap.edgeBetween(absDir, side)) != null) return null

            val tp = t.partMap(absDir)
            if (tp != null && tp.isInstanceOf[IConnectable]) return tp.asInstanceOf[IConnectable]
        }
        null
    }

    def getCenter(world:World, from:BlockCoord):IConnectable =
    {
        val t = PRLib.getMultipartTile(world, from)
        if (t != null)
        {
            val tp = t.partMap(6)
            if (tp != null && tp.isInstanceOf[IConnectable]) return tp.asInstanceOf[IConnectable]
        }
        null
    }
}

object BlockConnLib
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

    def getCorner(world:World, absDir:Int, edgeRot:Int, from:BlockCoord):IConnectable =
    {
        val pos = from.offset(absDir)
        val sideTo = Rotation.rotateSide(absDir^1, edgeRot)

        if (!WireConnLib.canConnectThroughCorner(world, pos, absDir^1, sideTo)) return null
        pos.offset(sideTo)

        val t = PRLib.getMultipartTile(world, pos)
        if (t != null)
        {
            val tp = t.partMap(absDir^1)
            if (tp != null && tp.isInstanceOf[IConnectable]) return tp.asInstanceOf[IConnectable]
        }
        null
    }

    def getStraight(world:World, absDir:Int, edgeRot:Int, from:BlockCoord):IConnectable =
    {
        val pos = from.offset(absDir)
        val t = PRLib.getMultipartTile(world, pos)

        if (t != null)
        {
            if (edgeRot > -1)
            {
                val sideOfConnPart = Rotation.rotateSide(absDir^1, edgeRot)
                val tp = t.partMap(sideOfConnPart)
                if (tp != null && tp.isInstanceOf[IConnectable]) return tp.asInstanceOf[IConnectable]
            }
            else
            {
                val tp = t.partMap(6)
                if (tp != null && tp.isInstanceOf[IConnectable]) return tp.asInstanceOf[IConnectable]
            }
        }
        null
    }
}
