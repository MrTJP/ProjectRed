/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.relocation

import mrtjp.core.world.WorldLib
import mrtjp.core.world.WorldLib._
import mrtjp.projectred.api.ITileMover
import net.minecraft.block.Block
import net.minecraft.block.state.IBlockState
import net.minecraft.init.Blocks
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.tileentity.TileEntity
import net.minecraft.util.EnumFacing
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World
import net.minecraftforge.fml.common.Loader

import scala.collection.immutable.ListMap
import scala.util.matching.Regex

object MovingTileRegistry extends ITileMover
{
    val rKeyVal:Regex = raw"([^\s]+.+[^\s]+)\s*->\s*([^\s]+.+[^\s]+)".r
    val rName:Regex = raw"([^\s]+.+[^\s]+)".r
    val rNameMetaM:Regex = raw"([^\s]+.+[^\s]+)m(\d+)".r
    val rMod:Regex = raw"mod:([^\s]+.+[^\s]+)".r

    var blockMetaMap:Map[IBlockState, ITileMover] = Map()
    var modMap:Map[String, ITileMover] = Map()

    var moverDescMap:Map[String, String] = Map()
    var moverNameMap:Map[String, ITileMover] = Map()

    var defaultMover:ITileMover = _
    var preferredMovers:Seq[(String, String)] = Seq()
    var mandatoryMovers:Seq[(String, String)] = Seq()

    def parseKV(kv:Seq[String]):Seq[(String, String)] =
        kv.map { case rKeyVal(k, v) => (k, v); case s => throw new MatchError(s"Illegal [k -> v] pair: $s") }

    def parseBlockMeta(b:String):Option[IBlockState] = b match {
        case rNameMetaM(name, meta) => Option(Block.getBlockFromName(name)).map(_.getStateFromMeta(meta.toInt))
        case rName(name) => Option(Block.getBlockFromName(name)).map(_.getDefaultState)
        case _ => throw new MatchError(s"Illegal set part: $b")
    }

    def parseAndSetMovers(kv:Seq[String]):Array[String] =
    {
        var moverMap = ListMap(parseKV(kv):_*)
        for ((k, v) <- preferredMovers) if (!moverMap.contains(k)) moverMap += k -> v
        for (pair <- mandatoryMovers) moverMap += pair
        moverMap.foreach(h => setMover(h._1, h._2))
        moverMap.map(p => p._1 + " -> " + p._2).toArray
    }

    def setMover(that:String, m:String)
    {
        if (!moverNameMap.contains(m)) return
        val h = moverNameMap(m)
        that match {
            case "default" => defaultMover = h
            case rMod(mod) if Loader.isModLoaded(mod) => modMap += mod -> h
            case _ => parseBlockMeta(that).foreach(blockMetaMap += _ -> h) // TODO: Throw error when this is None
        }
    }

    def registerTileMover(name:String, desc:String, m:ITileMover)
    {
        moverDescMap += name -> desc
        moverNameMap += name -> m
    }

    private def getHandler(state:IBlockState):ITileMover =
        blockMetaMap.getOrElse(state, blockMetaMap.getOrElse(state.getBlock.getDefaultState,
            modMap.getOrElse(state.getBlock.getRegistryName.getResourceDomain, defaultMover)))

    override def canMove(w:World, pos:BlockPos):Boolean =
        getHandler(w.getBlockState(pos)).canMove(w, pos)

    override def move(w:World, pos:BlockPos, side:EnumFacing):Unit =
        getHandler(w.getBlockState(pos)).move(w, pos, side)

    override def postMove(w:World, pos:BlockPos):Unit =
        getHandler(w.getBlockState(pos)).postMove(w, pos)

    // FIXME World#blockExists == World#isBlockLoaded?
    def canRunOverBlock(w:World, pos:BlockPos):Boolean =
        w.isBlockLoaded(pos) &&
                (w.isAirBlock(pos) || WorldLib.isBlockSoft(w, pos, w.getBlockState(pos)))
}

object CoordPushTileMover extends ITileMover
{
    override def canMove(w:World, pos:BlockPos) = true

    override def move(w:World, pos:BlockPos, side:EnumFacing)
    {
        val (state, te) = (w.getBlockState(pos), w.getTileEntity(pos))
        val pos2 = pos.offset(side)
        if (te != null) {
            te.invalidate()
            uncheckedRemoveTileEntity(w, pos)
        }
        uncheckedSetBlock(w, pos, Blocks.AIR.getDefaultState)
        uncheckedSetBlock(w, pos2, state)
        if (te != null) {
            te.setPos(pos2)
            te.validate()
            uncheckedSetTileEntity(w, pos2, te)
        }
    }

    override def postMove(w:World, pos:BlockPos){}
}

object SaveLoadTileMover extends ITileMover
{
    override def canMove(w:World, pos:BlockPos) = true

    override def move(w:World, pos:BlockPos, side:EnumFacing)
    {
        val (state, te) = (w.getBlockState(pos), w.getTileEntity(pos))
        val pos2 = pos.offset(side)
        val tag = if (te != null) {
            val tag = new NBTTagCompound
            te.writeToNBT(tag)
            tag.setInteger("x", pos2.getX)
            tag.setInteger("y", pos2.getY)
            tag.setInteger("z", pos2.getZ)
            te.onChunkUnload()
            w.removeTileEntity(pos)
            tag
        }
        else null
        uncheckedSetBlock(w, pos, Blocks.AIR.getDefaultState)
        uncheckedSetBlock(w, pos2, state)
        if (tag != null) {
            TileEntity.create(w, tag) match {
                case te:TileEntity => w.getChunkFromBlockCoords(pos2).addTileEntity(te)
                case _ =>
            }
        }
    }

    override def postMove(w:World, pos:BlockPos){}
}

object StaticTileMover extends ITileMover
{
    override def canMove(w:World, pos:BlockPos) = false

    override def move(w:World, pos:BlockPos, side:EnumFacing){}

    override def postMove(w:World, pos:BlockPos){}
}