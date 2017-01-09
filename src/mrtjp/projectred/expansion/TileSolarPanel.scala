/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import java.util

import codechicken.lib.lighting.LightModel
import codechicken.lib.raytracer.CuboidRayTraceResult
import codechicken.lib.render.item.IItemRenderer
import codechicken.lib.render.{CCModel, CCRenderState}
import codechicken.lib.texture.TextureUtils.IIconRegister
import codechicken.lib.util.TransformUtils
import codechicken.lib.vec._
import codechicken.lib.vec.uv.{MultiIconTransformation, UVTransformation}
import codechicken.microblock.FaceMicroFactory
import codechicken.multipart.{MultiPartRegistry, TItemMultiPart, TMultiPart}
import mrtjp.core.item.ItemCore
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.api.IConnectable
import mrtjp.projectred.core.libmc.PRLib
import mrtjp.projectred.core.{ILowLoadMachine, ILowLoadPowerLine, PowerConductor}
import net.minecraft.block.state.IBlockState
import net.minecraft.client.renderer.block.model.ItemCameraTransforms.TransformType
import net.minecraft.client.renderer.block.model.{BakedQuad, ItemCameraTransforms, ItemOverrideList}
import net.minecraft.client.renderer.texture.{TextureAtlasSprite, TextureMap}
import net.minecraft.client.renderer.vertex.DefaultVertexFormats
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.util.{BlockRenderLayer, EnumFacing, ITickable, ResourceLocation}
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World
import net.minecraftforge.client.model.IPerspectiveAwareModel
import net.minecraftforge.client.model.IPerspectiveAwareModel.MapWrapper
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

import scala.collection.JavaConversions._

class SolarPanelPart extends TMultiPart with TFaceElectricalDevice with ILowLoadMachine with ITickable
{
    val cond = new PowerConductor(this, 0 until 4)
    {
        override def capacitance = 4.0
    }

    override def getType = SolarPanelPart.typeID

    override def getItem = new ItemStack(ProjectRedExpansion.itemSolar)

    override def conductor(dir:Int) = cond

    override def getBounds = FaceMicroFactory.aBounds(0x10|side)
    override def getOcclusionBoxes = SolarPanelPart.oBoxes(side).toSeq

    override def doesRotate = false

    override def canConnectPart(part:IConnectable, r:Int) = part match
    {
        case t:ILowLoadMachine => true
        case t:ILowLoadPowerLine => true
        case _ => false
    }

    override def update()
    {
        if (!world.isRemote)
        {
            cond.update()
            if (cond.voltage() < 100.0)
            {
                val I = 2.5*heightMultiplier*timeOfDayMultiplier*sideMultiplier*rainMultiplier*visibilityMultiplier
                cond.applyCurrent(I)
            }
        }
    }

    def heightMultiplier = 0.90+0.10*y/256.0

    def timeOfDayMultiplier =
    {
        val t = world.getWorldTime%24000
        if (t > 12000) 0.0
        else 0.50+0.50*math.sin(math.Pi*t/12000.0)
    }

    def sideMultiplier = side match
    {
        case 0 => 1.0
        case 1 => 0.0
        case 2|3 => 0.4
        case 4|5 => 0.3
    }

    def rainMultiplier = 1.0-world.rainingStrength

    def visibilityMultiplier =
        if (tile.partMap(1) != null) 0.0
        else if (world.canSeeSky(pos)) 1.0
        else if (world.canSeeSky(pos.up()) && !world.getBlockState(pos.up).getMaterial.isOpaque) 0.7
        else 0.0

    @SideOnly(Side.CLIENT)
    override def renderStatic(position:Vector3, layer:BlockRenderLayer, ccrs: CCRenderState) =
    {
        if (layer == BlockRenderLayer.SOLID)
        {
            ccrs.setBrightness(world, pos)
            RenderSolarPanel.render(ccrs, side, position)
            true
        }
        else false
    }

    @SideOnly(Side.CLIENT)
    override def getBrokenIcon(side:Int) =
        if (side == 1) RenderSolarPanel.top else RenderSolarPanel.side

    @SideOnly(Side.CLIENT)
    override def getBreakingIcon(hit: CuboidRayTraceResult): TextureAtlasSprite = null
}

object SolarPanelPart
{
    val typeID = "projectred-expansion:solar_panel"

    var oBoxes = Array.ofDim[Cuboid6](6, 2)

    oBoxes(0)(0) = new Cuboid6(1 / 8D, 0, 0, 7 / 8D, 1 / 8D, 1)
    oBoxes(0)(1) = new Cuboid6(0, 0, 1 / 8D, 1, 1 / 8D, 7 / 8D)
    for (s <- 1 until 6)
    {
        val t = Rotation.sideRotations(s).at(Vector3.center)
        oBoxes(s)(0) = oBoxes(0)(0).copy.apply(t)
        oBoxes(s)(1) = oBoxes(0)(1).copy.apply(t)
    }
}

class ItemSolarPanel extends ItemCore with TItemMultiPart //with TItemGlassSound
{
    setCreativeTab(ProjectRedExpansion.tabExpansion)

    override def newPart(item:ItemStack, player:EntityPlayer, world:World, pos:BlockPos, side:Int, vhit:Vector3):TMultiPart =
    {
        val onPos = pos.offset(EnumFacing.VALUES(side^1))
        if (!PRLib.canPlaceGateOnSide(world, onPos, side)) return null

        val solar = MultiPartRegistry.loadPart(SolarPanelPart.typeID, null).asInstanceOf[SolarPanelPart]
        if (solar != null) solar.preparePlacement(player, pos, side, item.getItemDamage)
        solar

    }
}

object RenderSolarPanel extends IItemRenderer with IIconRegister with IPerspectiveAwareModel
{
    var side:TextureAtlasSprite = _
    var top:TextureAtlasSprite = _
    var bottom:TextureAtlasSprite = _

    var iconT:UVTransformation = _

    val models =
    {
        val array = new Array[CCModel](6)
        val m = CCModel.quadModel(24)
        m.generateBlock(0,  new Cuboid6(0, 0, 0, 1, 2/16D, 1).expand(-0.0005), 0)
        for (s <- 0 until 6)
        {
            val m2 = m.copy.apply(Rotation.sideRotations(s) at Vector3.center)
            m2.computeNormals()
            m2.shrinkUVs(0.0005)
            m2.computeLighting(LightModel.standardLightModel)
            array(s) = m2
        }
        array
    }

    def render(ccrs:CCRenderState, side:Int, pos:Vector3)
    {
        models(side).render(ccrs, iconT, pos.translation)
    }

    override def registerIcons(reg:TextureMap)
    {
        side = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/solar/side"))
        top = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/solar/top"))
        bottom = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/solar/bottom"))
        iconT = new MultiIconTransformation(bottom, top, side, side, side, side)
    }

    override def renderItem(item: ItemStack) = {
        val ccrs = CCRenderState.instance()
        ccrs.reset()
        ccrs.pullLightmap()
        ccrs.startDrawing(0x07, DefaultVertexFormats.ITEM)
        models(0).render(ccrs, iconT)
        ccrs.draw()
    }
    //TODO Trait in CCL scala libs for this mundane stuff.
    override def handlePerspective(cameraTransformType: TransformType) = MapWrapper.handlePerspective(this, TransformUtils.DEFAULT_BLOCK, cameraTransformType)

    override def getQuads(state: IBlockState, side: EnumFacing, rand: Long): util.List[BakedQuad] = new util.ArrayList[BakedQuad]()

    override def isAmbientOcclusion: Boolean = true

    override def isGui3d: Boolean = true

    override def isBuiltInRenderer: Boolean = true

    override def getParticleTexture: TextureAtlasSprite = null

    override def getItemCameraTransforms: ItemCameraTransforms = ItemCameraTransforms.DEFAULT

    override def getOverrides: ItemOverrideList = ItemOverrideList.NONE
}
