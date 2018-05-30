/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.relocation

import codechicken.lib.lighting.LightModel
import codechicken.lib.model.ModelRegistryHelper
import codechicken.lib.render.CCModel._
import codechicken.lib.render.block.{BlockRenderingRegistry, ICCBlockRenderer}
import codechicken.lib.render.item.IItemRenderer
import codechicken.lib.render.{CCModel, CCRenderState, OBJParser}
import codechicken.lib.texture.TextureUtils.IIconRegister
import codechicken.lib.util.TransformUtils
import codechicken.lib.vec.{Rotation, _}
import codechicken.lib.vec.uv.IconTransformation
import codechicken.multipart.{MultiPartRegistry, TItemMultiPart, TileMultipart}
import mrtjp.core.vec.ModelRayTracer
import mrtjp.projectred.ProjectRedRelocation
import mrtjp.projectred.api.IFrame
import net.minecraft.block.material.Material
import net.minecraft.block.state.IBlockState
import net.minecraft.block.{Block, SoundType}
import net.minecraft.client.renderer.BufferBuilder
import net.minecraft.client.renderer.block.model.ItemCameraTransforms
import net.minecraft.client.renderer.texture.{TextureAtlasSprite, TextureMap}
import net.minecraft.client.renderer.vertex.DefaultVertexFormats
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.{Item, ItemBlock, ItemStack}
import net.minecraft.util.math.{AxisAlignedBB, BlockPos, Vec3d}
import net.minecraft.util._
import net.minecraft.world.{IBlockAccess, World}
import net.minecraftforge.fml.relauncher.{Side, SideOnly}
import org.lwjgl.opengl.GL11

import scala.collection.JavaConversions._

class BlockFrame extends Block(Material.WOOD) with IFrame
{
    setResistance(5.0F)
    setHardness(2.0F)
    setSoundType(SoundType.WOOD)
    setCreativeTab(ProjectRedRelocation.tabRelocation)

    override def stickOut(w:World, pos:BlockPos, side:EnumFacing) = true

    override def stickIn(w:World, pos:BlockPos, side:EnumFacing) = true

    override def isOpaqueCube(state:IBlockState):Boolean = false

    override def isNormalCube(state:IBlockState):Boolean = false

    override protected def rayTrace(pos:BlockPos, start:Vec3d, end:Vec3d, boundingBox:AxisAlignedBB) = FrameRenderer.raytrace(pos, 0, start, end)

    override def isSideSolid(base_state:IBlockState, world:IBlockAccess, pos:BlockPos, side:EnumFacing) = false

    @SideOnly(Side.CLIENT)
    override def getRenderType(state:IBlockState) = FrameRenderer.renderType
}

class ItemBlockFrame(block:Block) extends ItemBlock(block)
{
    def getHitDepth(vhit: Vector3, side: Int): Double =
        vhit.copy.scalarProject(Rotation.axes(side)) + (side % 2 ^ 1)

    override def onItemUse(player:EntityPlayer, world:World, bpos:BlockPos, hand:EnumHand, facing:EnumFacing, hitX:Float, hitY:Float, hitZ:Float):EnumActionResult =
    {
        val stack = player.getHeldItem(hand)
        var pos = new BlockPos(bpos)
        val side = facing.getIndex
        val vhit = new Vector3(hitX, hitY, hitZ)
        val d = getHitDepth(vhit, side)

        def place(): EnumActionResult = {
            val part = newPart(stack, player, world, pos, side, vhit)
            if (part == null || !TileMultipart.canPlacePart(world, pos, part)) return EnumActionResult.FAIL

            if (!world.isRemote) {
                TileMultipart.addPart(world, pos, part)
                val sound = getPlacementSound(stack)
                if (sound != null) {
                    world.playSound(null, bpos, sound.getPlaceSound,
                        SoundCategory.BLOCKS, (sound.getVolume + 1.0F) / 2.0F, sound.getPitch * 0.8F)
                }
            }
            if (!player.capabilities.isCreativeMode) stack.shrink(1)
            EnumActionResult.SUCCESS
        }

        if (d < 1 && place() == EnumActionResult.SUCCESS) return EnumActionResult.SUCCESS

        if (super.onItemUse(player, world, bpos, hand, facing, hitX, hitY, hitZ) == EnumActionResult.SUCCESS)
            return EnumActionResult.SUCCESS

        pos = pos.offset(facing)
        place()
    }

    def newPart(item:ItemStack, player:EntityPlayer, world:World, pos:BlockPos, side:Int, vhit:Vector3) =
        MultiPartRegistry.loadPart(FramePart.partType, null)

    def getPlacementSound(item:ItemStack) = SoundType.WOOD

    override def canPlaceBlockOnSide(worldIn:World, pos:BlockPos, side:EnumFacing, player:EntityPlayer, stack:ItemStack) = true
}

object FrameRenderer extends ICCBlockRenderer with IIconRegister with IItemRenderer
{
    val renderType = BlockRenderingRegistry.createRenderType("projectred-relocation:frame")

    private var icon:TextureAtlasSprite = _
    private var iconT:IconTransformation = _

    private val modelParts = OBJParser.parseModels(new ResourceLocation(
        "projectred:textures/obj/mechanical/frame.obj"), GL11.GL_QUADS, null).map(a => (a._1, a._2.backfacedCopy))

    private val models = new Array[CCModel](64)

    def init()
    {
        BlockRenderingRegistry.registerRenderer(renderType, this)
        ModelRegistryHelper.registerItemRenderer(Item.getItemFromBlock(ProjectRedRelocation.blockFrame), this)
    }

    override def renderItem(stack:ItemStack, transformType:ItemCameraTransforms.TransformType)
    {
        val ccrs = CCRenderState.instance()
        ccrs.reset()
        ccrs.pullLightmap()
        ccrs.startDrawing(0x07, DefaultVertexFormats.ITEM)
        getOrGenerateModel(0).render(ccrs, iconT)
        ccrs.draw()
    }

    override def getTransforms = TransformUtils.DEFAULT_BLOCK

    override def isAmbientOcclusion = true

    override def isGui3d = true

    override def renderBlock(world:IBlockAccess, pos:BlockPos, state:IBlockState, buffer:BufferBuilder) =
    {
        val ccrs = CCRenderState.instance()
        ccrs.reset()
        ccrs.bind(buffer)
        ccrs.lightMatrix.locate(world, pos)

        ccrs.setBrightness(world, pos)
        render(ccrs, Vector3.fromBlockPos(pos), 0)
        true
    }

    override def handleRenderBlockDamage(world:IBlockAccess, pos:BlockPos, state:IBlockState, sprite:TextureAtlasSprite, buffer:BufferBuilder)
    {
        val ccrs = CCRenderState.instance()
        ccrs.reset()
        ccrs.bind(buffer)

        getOrGenerateModel(0).render(ccrs, new Translation(pos), new IconTransformation(sprite))
    }

    override def renderBrightness(state:IBlockState, brightness:Float){}

    override def registerTextures(map:TextureMap){}

    override def registerIcons(textureMap:TextureMap)
    {
        icon = textureMap.registerSprite(new ResourceLocation("projectred:blocks/mechanical/frame"))

        iconT = new IconTransformation(icon)
    }

    def raytrace(pos:BlockPos, mask:Int, start:Vec3d, end:Vec3d) =
        ModelRayTracer.raytraceModel(pos.getX, pos.getY, pos.getZ, start, end, getOrGenerateModel(mask))

    def render(ccrs:CCRenderState, pos:Vector3, mask:Int)
    {
        getOrGenerateModel(mask).render(ccrs, pos.translation, iconT)
    }

    def getOrGenerateModel(mask:Int) =
    {
        var m = models(mask&0x3F)
        if (m == null) {
            m = generateModel(mask)
            models(mask&0x3F) = m
        }
        m
    }

    def generateModel(mask:Int) =
    {
        var m = modelParts("frame").copy

        for (s <- 0 until 6) if ((mask & 1 << s) == 0)
            m = combine(Seq(m, modelParts("cross_" + s)))

        finishModel(m)
    }

    def finishModel(m:CCModel) =
    {
        m.shrinkUVs(0.0005)
        m.computeNormals()
        m.computeLighting(LightModel.standardLightModel)
    }
}