/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.relocation

import java.util.{List => JList}

import codechicken.lib.vec.{Cuboid6, Vector3}
import mrtjp.core.block.{MTBlockTile, MultiTileBlock}
import mrtjp.projectred.ProjectRedRelocation
import net.minecraft.block.SoundType
import net.minecraft.block.material.Material
import net.minecraft.block.state.{BlockFaceShape, IBlockState}
import net.minecraft.entity.{Entity, MoverType}
import net.minecraft.util.EnumBlockRenderType
import net.minecraft.util.math.AxisAlignedBB
import net.minecraft.world.World

import scala.collection.JavaConversions._

class BlockMovingRow extends MultiTileBlock(Material.IRON)
{
    setHardness(-1F)
    setSoundType(SoundType.GROUND)
    setCreativeTab(null)

    override def getRenderType(state:IBlockState):EnumBlockRenderType = EnumBlockRenderType.INVISIBLE
}

object TileMovingRow
{
    def setBlockForRow(w:World, r:BlockRow)
    {
        w.setBlockState(r.pos, ProjectRedRelocation.blockMovingRow.getDefaultState, 3)
    }

    def getBoxFor(w:World, r:BlockRow, progress:Double):Cuboid6 =
    {
        val p = r.pos.offset(r.moveDir)
        val bl = w.getBlockState(p)

        bl.getCollisionBoundingBox(w, p) match {
            case aabb:AxisAlignedBB => new Cuboid6(aabb).subtract(Vector3.fromBlockPos(r.pos))
                    .add(Vector3.fromVec3i(r.moveDir.getDirectionVec) * progress)
            case _ => Cuboid6.full.copy
        }
    }
}

class TileMovingRow extends MTBlockTile
{
    var prevProg = 0.0

    override def updateServer():Unit =
    {
        if (!MovementManager.isMoving(world, pos)) world.setBlockToAir(pos)
    }

    override def getBlock:BlockMovingRow = ProjectRedRelocation.blockMovingRow

    override def getBlockBounds:Cuboid6 =
    {
        val s = MovementManager.getEnclosedStructure(world, pos)
        if (s != null) {
            val r = s.rows.find(_.contains(pos)).get
            TileMovingRow.getBoxFor(world, r, s.progress)
        }
        else Cuboid6.full
    }

    override def getCollisionBounds:Cuboid6 = getBlockBounds

    override def getBlockFaceShape(side:Int) = BlockFaceShape.UNDEFINED

    def pushEntities(r:BlockRow, progress:Double)
    {
        val box = Cuboid6.full.copy.add(Vector3.fromBlockPos(r.preMoveBlocks.head))
                .add(Vector3.fromVec3i(r.moveDir.getDirectionVec).multiply(progress))
        val boxBounds = box.aabb()

        val dp = (if (progress >= 1.0) progress + 0.1 else progress) - prevProg
        val d = Vector3.fromVec3i(r.moveDir.getDirectionVec) * dp
        world.getEntitiesWithinAABBExcludingEntity(null, boxBounds) match {
            case list:JList[_] =>
                for (e <- list.asInstanceOf[JList[Entity]]) {
                    e.move(MoverType.PISTON, d.x, d.y*4 max 0, d.z) //TODO find better way to do this
                }
            case _ =>
        }

        prevProg = progress
    }
}