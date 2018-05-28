/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.relocation

import codechicken.lib.raytracer.{CuboidRayTraceResult, IndexedCuboid6}
import codechicken.lib.render.CCRenderState
import codechicken.lib.texture.TextureUtils
import codechicken.lib.vec.Rotation._
import codechicken.lib.vec.Vector3._
import codechicken.lib.vec.{Cuboid6, Vector3}
import codechicken.microblock.CommonMicroblock
import codechicken.multipart._
import codechicken.multipart.api.IPartConverter
import codechicken.multipart.handler.MultipartProxy
import mrtjp.core.world.WorldLib
import mrtjp.projectred.ProjectRedRelocation
import mrtjp.projectred.api.{IFrame, ITileMover}
import net.minecraft.block.state.IBlockState
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.init.Blocks
import net.minecraft.item.ItemStack
import net.minecraft.util._
import net.minecraft.util.math.{BlockPos, RayTraceResult, Vec3d}
import net.minecraft.world.World
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

import scala.collection.JavaConversions._

class FramePart extends TMultiPart with IFrame with TCuboidPart with TNormalOcclusionPart with TIconHitEffectsPart// with ICapabilityProvider
{
    override val getType = FramePart.partType

    override def stickOut(w:World, pos:BlockPos, side:EnumFacing) = tile.partMap(side.ordinal) match {
        case part:CommonMicroblock => part.getSize != 1
        case _ => true
    }

    override def stickIn(w:World, pos:BlockPos, side:EnumFacing) = tile.partMap(side.ordinal) match {
        case part:CommonMicroblock => part.getSize != 1
        case _ => true
    }

    override def getStrength(player:EntityPlayer, hit:CuboidRayTraceResult) =
        player.getDigSpeed(ProjectRedRelocation.blockFrame.getDefaultState, new BlockPos(0, -1, 0))

    override def getDrops = Seq(new ItemStack(ProjectRedRelocation.blockFrame))
    override def pickItem(hit:CuboidRayTraceResult) = new ItemStack(ProjectRedRelocation.blockFrame)

    override def getBounds = Cuboid6.full

    override def getOcclusionBoxes =
    {
        FramePart.sideOccludeTest match
        {
            case -1 => Seq()
            case s => Seq(FramePart.aBounds(s))
        }
    }

    def sideOcclusionTest(side:Int) =
    {
        FramePart.sideOccludeTest = side
        val fits = tile.canReplacePart(this, this)
        FramePart.sideOccludeTest = -1
        fits
    }

    def sideOcclusionMask =
    {
        var mask = 0
        for (s <- 0 until 6) if (sideOcclusionTest(s)) mask |= 1<<s
        mask
    }

    override def occlusionTest(npart:TMultiPart):Boolean =
    {
        if (npart.isInstanceOf[FramePart]) return false

        //modified normal occlusion test that also tests collision boxes
        if (FramePart.sideOccludeTest != -1)
        {
            var boxes = Seq[Cuboid6]()
            if(npart.isInstanceOf[TNormalOcclusionPart])
                boxes ++= npart.asInstanceOf[TNormalOcclusionPart].getOcclusionBoxes
            if(npart.isInstanceOf[TPartialOcclusionPart])
                boxes ++= npart.asInstanceOf[TPartialOcclusionPart].getPartialOcclusionBoxes
            boxes ++= npart.getCollisionBoxes

            NormalOcclusionTest(boxes, getOcclusionBoxes)
        }
        else super.occlusionTest(npart)
    }

    override def collisionRayTrace(start:Vec3d, end:Vec3d) =
    {
        FrameRenderer.raytrace(pos, ~sideOcclusionMask,  start, end) match {
            case mop:RayTraceResult =>
                val cube = new IndexedCuboid6(0, Cuboid6.full)
                val dist = start.squareDistanceTo(mop.hitVec)
                new CuboidRayTraceResult(new Vector3(mop.hitVec), mop.getBlockPos, mop.sideHit, cube, dist)
            case _ => null
        }
    }

    override def renderStatic(pos:Vector3, layer:BlockRenderLayer, ccrs:CCRenderState) = layer match {
        case BlockRenderLayer.CUTOUT =>
            ccrs.setBrightness(world, this.pos)
            FrameRenderer.render(ccrs, pos, ~sideOcclusionMask)
            true
        case _ => super.renderStatic(pos, layer, ccrs)
    }

    @SideOnly(Side.CLIENT)
    override def getBreakingIcon(hit:CuboidRayTraceResult) = getBrokenIcon(hit.sideHit.ordinal)

    @SideOnly(Side.CLIENT)
    override def getBrokenIcon(side:Int) =
        TextureUtils.getParticleIconForBlock(ProjectRedRelocation.blockFrame.getDefaultState)

//    override def hasCapability(capability: Capability[_], facing: EnumFacing) = capability == CapabilityFrame.CAPABILITY
//
//    override def getCapability[T](capability: Capability[T], facing: EnumFacing):T = {
//        if (capability == CapabilityFrame.CAPABILITY) this.asInstanceOf[T]
//        else null.asInstanceOf[T]
//    }
}

object FramePart
{
    val partType = new ResourceLocation("projectred-relocation:partFrame")

    var sideOccludeTest = -1

    var aBounds = new Array[Cuboid6](6)

    {
        val i = 4/16D
        val th = 1/16D
        for(s <- 0 until 6)
            aBounds(s) = new Cuboid6(i, 0, i, 1-i, th, 1-i)
                    .apply(sideRotations(s).at(center))
    }
}

object FrameBlockConverter extends IPartConverter
{
    override def canConvert(world:World, pos:BlockPos, state:IBlockState) =
        state == ProjectRedRelocation.blockFrame.getDefaultState

    override def convert(world:World, pos:BlockPos, state:IBlockState) = new FramePart
}

object FMPTileHandler extends ITileMover
{
    override def canMove(w:World, pos:BlockPos) = w.getTileEntity(pos) match {
        case t:TileMultipart => true
        case _ => false
    }

    override def move(w:World, pos:BlockPos, dir:EnumFacing) {
        w.getTileEntity(pos) match {
            case t:TileMultipart =>
                t.invalidate()
                WorldLib.uncheckedRemoveTileEntity(w, pos)
                WorldLib.uncheckedSetBlock(w, pos, Blocks.AIR.getDefaultState)

                val pos2 = pos.offset(dir)
                WorldLib.uncheckedSetBlock(w, pos2, MultipartProxy.block.getDefaultState)
                t.setPos(pos2)
                t.validate()
                WorldLib.uncheckedSetTileEntity(w, pos2, t)
            case _ =>
        }
    }

    override def postMove(w:World, pos:BlockPos) = WorldLib.uncheckedGetTileEntity(w, pos) match {
        case te:TileMultipart => te.onMoved()
        case _ =>
    }
}