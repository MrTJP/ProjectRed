/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.raytracer.VoxelShapeCache
import codechicken.lib.render.item.IItemRenderer
import codechicken.lib.render.lighting.LightModel
import codechicken.lib.render.{CCModel, CCRenderState}
import codechicken.lib.texture.{AtlasRegistrar, IIconRegister}
import codechicken.lib.util.TransformUtils
import codechicken.lib.vec._
import codechicken.lib.vec.uv.{MultiIconTransformation, UVTransformation}
import codechicken.microblock.FaceMicroFactory
import codechicken.multipart.api.part.{ITickablePart, TMultiPart}
import codechicken.multipart.api.{ItemMultiPart, MultiPartType}
import codechicken.multipart.util.PartRayTraceResult
import com.google.common.collect.ImmutableSet
import com.google.gson.{JsonDeserializationContext, JsonObject}
import com.mojang.blaze3d.matrix.MatrixStack
import com.mojang.datafixers.util.Pair
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.api.IConnectable
import mrtjp.projectred.core._
import net.minecraft.client.renderer.model._
import net.minecraft.client.renderer.texture.TextureAtlasSprite
import net.minecraft.client.renderer.{IRenderTypeBuffer, RenderType}
import net.minecraft.item.{Item, ItemStack, ItemUseContext}
import net.minecraft.nbt.CompoundNBT
import net.minecraft.resources.IResourceManager
import net.minecraft.util.ResourceLocation
import net.minecraft.util.math.shapes.{ISelectionContext, VoxelShape}
import net.minecraft.world.World
import net.minecraftforge.api.distmarker.{Dist, OnlyIn}
import net.minecraftforge.client.model.geometry.IModelGeometry
import net.minecraftforge.client.model.{IModelConfiguration, IModelLoader}
import net.minecraftforge.resource.IResourceType

import java.util
import java.util.Collections
import java.util.function.{Function, Predicate}

class SolarPanelPart extends TMultiPart with TFaceElectricalDevice with ILowLoadMachine with ITickablePart
{
    val cond = new PowerConductor(this, 0 until 4) {
        override def capacitance = 4.0
    }

    override def save(tag:CompoundNBT):Unit = {
        super.save(tag)
        cond.save(tag)
    }

    override def load(tag:CompoundNBT):Unit = {
        super.load(tag)
        cond.load(tag)
    }
    override def connWorld:World = world

    override def getType:MultiPartType[_] = ExpansionContent.solarPanelPart.get

    override def getItem = new ItemStack(ExpansionContent.solarPanelItem.get)

    override def conductor(dir:Int) = cond

    override def getBounds:Cuboid6 = FaceMicroFactory.aBounds(0x10|side)

    override def getShape(context:ISelectionContext):VoxelShape = FaceMicroFactory.aShapes(0x10|side)

    override def getOcclusionShape:VoxelShape = SolarPanelPart.oShapes(side)

    override def doesRotate = false

    override def canConnectPart(part:IConnectable, r:Int):Boolean = part match {
        case t:ILowLoadMachine => true
        case t:ILowLoadPowerLine => true
        case _ => false
    }

    override def tick():Unit = {
        if (!world.isClientSide) {
            cond.update()
            if (cond.voltage() < 100.0) {
                val I = 2.5*heightMultiplier*timeOfDayMultiplier*sideMultiplier*rainMultiplier*visibilityMultiplier
                cond.applyCurrent(I)
            }
        }
    }

    def heightMultiplier:Double = 0.90+0.10*pos.getY/256.0

    def timeOfDayMultiplier:Double = {
        val t = world.getDayTime%24000
        if (t > 12000) 0.0
        else 0.50+0.50*math.sin(math.Pi*t/12000.0)
    }

    def sideMultiplier:Double = side match {
        case 0 => 1.0
        case 1 => 0.0
        case 2|3 => 0.4
        case 4|5 => 0.3
    }

    def rainMultiplier:Double = 1.0-world.rainLevel

    def visibilityMultiplier:Double =
        if (tile.getSlottedPart(1) != null) 0.0
        else if (world.canSeeSky(pos)) 1.0
        else if (world.canSeeSky(pos.above()) && !world.getBlockState(pos.above()).getMaterial.isSolidBlocking) 0.7
        else 0.0

    override def renderStatic(layer:RenderType, ccrs:CCRenderState):Boolean = {
        if (layer == null || (layer == RenderType.cutout() && Configurator.staticGates)) {
            ccrs.setBrightness(world, this.pos)
            RenderSolarPanel.render(ccrs, side, Vector3.ZERO)
            true
        } else
            false
    }

    @OnlyIn(Dist.CLIENT)
    override def getBreakingIcon(hit:PartRayTraceResult):TextureAtlasSprite =
        getBrokenIcon(hit.getDirection.ordinal)

    @OnlyIn(Dist.CLIENT)
    override def getBrokenIcon(side:Int):TextureAtlasSprite =
        if (side == 1) RenderSolarPanel.top else RenderSolarPanel.side
}

object SolarPanelPart
{
    var oBoxes:Array[Array[Cuboid6]] = Array.ofDim[Cuboid6](6, 2)
    val oShapes:Array[VoxelShape] = new Array[VoxelShape](6)

    for (s <- 0 until 6) {
        val t = Rotation.sideRotations(s).at(Vector3.CENTER)
        val occlusion1 = new Cuboid6(1 / 8D, 0, 0, 7 / 8D, 1 / 8D, 1)
        val occlusion2 = new Cuboid6(0, 0, 1 / 8D, 1, 1 / 8D, 7 / 8D)


        oBoxes(s)(0) = occlusion1.apply(t)
        oBoxes(s)(1) = occlusion2.apply(t)
        oShapes(s) = VoxelShapeCache.merge(ImmutableSet.copyOf(oBoxes(s).map(VoxelShapeCache.getShape)))
    }
}

class ItemSolarPanel extends ItemMultiPart(new Item.Properties().tab(ExpansionContent.expansionItemGroup))
{
    override def newPart(context:ItemUseContext):TMultiPart = {
        val side = context.getClickedFace
        val onPos = context.getClickedPos.relative(side.getOpposite)
        if (!PRLib.canPlaceGateOnSide(context.getLevel, onPos, side)) {
            null
        } else {
            val part = ExpansionContent.solarPanelPart.get.createPartServer(null).asInstanceOf[SolarPanelPart]
            part.preparePlacement(context.getPlayer, onPos, side.ordinal)
            part
        }
    }
}

object RenderSolarPanel extends IItemRenderer with IIconRegister
{
    var side:TextureAtlasSprite = _
    var top:TextureAtlasSprite = _
    var bottom:TextureAtlasSprite = _

    var iconT:UVTransformation = _

    val models:Array[CCModel] =  {
        val array = new Array[CCModel](6)
        val m = CCModel.quadModel(24)
        m.generateBlock(0,  new Cuboid6(0, 0, 0, 1, 2/16D, 1).expand(-0.0005), 0)
        for (s <- 0 until 6)
        {
            val m2 = m.copy.apply(Rotation.sideRotations(s) at Vector3.CENTER)
            m2.computeNormals()
            m2.shrinkUVs(0.0005)
            m2.computeLighting(LightModel.standardLightModel)
            array(s) = m2
        }
        array
    }

    def render(ccrs:CCRenderState, s:Int, pos:Vector3):Unit = {
        if (iconT == null)
            iconT = new MultiIconTransformation(bottom, top, side, side, side, side)
        models(s).render(ccrs, iconT, pos.translation)
    }

    override def registerIcons(reg:AtlasRegistrar):Unit = {
        reg.registerSprite(new ResourceLocation(ProjectRedExpansion.MOD_ID, "block/solar_panel/side"), side = _)
        reg.registerSprite(new ResourceLocation(ProjectRedExpansion.MOD_ID, "block/solar_panel/top"), top = _)
        reg.registerSprite(new ResourceLocation(ProjectRedExpansion.MOD_ID, "block/solar_panel/bottom"), bottom = _)
    }

    override def useAmbientOcclusion = true
    override def isGui3d = true
    override def getModelTransform:IModelTransform = TransformUtils.DEFAULT_BLOCK
    override def usesBlockLight() = true

    override def renderItem(stack:ItemStack, transformType:ItemCameraTransforms.TransformType, mStack:MatrixStack, getter:IRenderTypeBuffer, packedLight:Int, packedOverlay:Int):Unit =
    {
        if (iconT == null)
            iconT = new MultiIconTransformation(bottom, top, side, side, side, side)

        val ccrs = CCRenderState.instance()
        ccrs.reset()
        ccrs.brightness = packedLight
        ccrs.overlay = packedOverlay
        ccrs.bind(RenderType.cutout(), getter, mStack)
        models(0).render(ccrs, iconT)
    }

    class Loader extends IModelLoader[Loader] with IModelGeometry[Loader] {
        override def read(deserializationContext: JsonDeserializationContext, modelContents: JsonObject) = this

        override def getTextures(owner: IModelConfiguration, modelGetter: Function[ResourceLocation, IUnbakedModel], missingTextureErrors: util.Set[Pair[String, String]]) = Collections.emptyList()

        override def bake(owner: IModelConfiguration, bakery: ModelBakery, spriteGetter: Function[RenderMaterial, TextureAtlasSprite], modelTransform: IModelTransform, overrides: ItemOverrideList, modelLocation: ResourceLocation) = RenderSolarPanel

        // Following 2 methods included because of compile issues
        override def onResourceManagerReload(resourceManager:IResourceManager, resourcePredicate:Predicate[IResourceType]):Unit = {}
        override def onResourceManagerReload(resourceManager:IResourceManager):Unit = {}
    }
}
