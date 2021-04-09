package mrtjp.projectred.transmission

import codechicken.lib.render._
import codechicken.lib.render.item.IItemRenderer
import codechicken.lib.render.pipeline.IVertexOperation
import codechicken.lib.util.{SneakyUtils, TransformUtils}
import codechicken.lib.vec.uv.IconTransformation
import com.google.gson.{JsonDeserializationContext, JsonObject}
import com.mojang.blaze3d.matrix.MatrixStack
import com.mojang.datafixers.util.Pair
import net.minecraft.client.renderer.model.ItemCameraTransforms.TransformType
import net.minecraft.client.renderer.model._
import net.minecraft.client.renderer.texture.TextureAtlasSprite
import net.minecraft.client.renderer.{IRenderTypeBuffer, RenderType}
import net.minecraft.item.ItemStack
import net.minecraft.resources.IResourceManager
import net.minecraft.util.ResourceLocation
import net.minecraftforge.client.model.geometry.IModelGeometry
import net.minecraftforge.client.model.{IModelConfiguration, IModelLoader}
import net.minecraftforge.resource.{IResourceType, SelectiveReloadStateHandler}

import java.util
import java.util.Collections
import java.util.function.{Predicate, Function => JFunc}

trait TWireItemRenderCommon extends IItemRenderer {
    override def isAmbientOcclusion = false

    override def isGui3d = true

    override def getTransforms = TransformUtils.DEFAULT_BLOCK

    override def func_230044_c_ = true

    override def renderItem(stack: ItemStack, transformType: TransformType, mStack: MatrixStack, getter: IRenderTypeBuffer, packedLight: Int, packedOverlay: Int) = {
        stack.getItem match {
            case item: ItemPartWire =>
                val ccrs = CCRenderState.instance()
                ccrs.reset()
                ccrs.brightness = packedLight
                ccrs.overlay = packedOverlay
                renderWireInventory(stack, item.wireType, ccrs, transformType, mStack, getter)
            case _ =>
        }
    }

    def renderWireInventory(stack: ItemStack, wireType: WireType, ccrs: CCRenderState, transformType: TransformType, mStack: MatrixStack, renderTypes: IRenderTypeBuffer) {

        ccrs.bind(RenderType.getCutout, renderTypes, mStack)
        doRender(wireType.getThickness, wireType.getItemColour << 8 | 0xFF, ccrs, new IconTransformation(wireType.getTextures.get(0)))
    }

    def doRender(thickness: Int, renderHue: Int, ccrs: CCRenderState, ops: IVertexOperation*)
}

object WireItemRenderer extends TWireItemRenderCommon {

    override def doRender(thickness: Int, renderHue: Int, ccrs: CCRenderState, ops: IVertexOperation*) {
        RenderWire.renderInv(thickness, renderHue, ccrs, ops: _*)
    }
}

object FramedWireItemRenderer extends TWireItemRenderCommon {

    override def doRender(thickness: Int, renderHue: Int, ccrs: CCRenderState, ops: IVertexOperation*) {
        RenderFramedWire.renderInv(thickness, renderHue, ccrs, ops: _*)
    }
}

class WireModelLoader extends IModelLoader[WireModelLoader] with IModelGeometry[WireModelLoader] {
    override def read(deserializationContext: JsonDeserializationContext, modelContents: JsonObject) = this

    override def getTextures(owner: IModelConfiguration, modelGetter: JFunc[ResourceLocation, IUnbakedModel], missingTextureErrors: util.Set[Pair[String, String]]) = Collections.emptyList()

    override def bake(owner: IModelConfiguration, bakery: ModelBakery, spriteGetter: JFunc[Material, TextureAtlasSprite], modelTransform: IModelTransform, overrides: ItemOverrideList, modelLocation: ResourceLocation) = WireItemRenderer

    // Following 2 methods included because of compile issues
    override def onResourceManagerReload(resourceManager:IResourceManager, resourcePredicate:Predicate[IResourceType]):Unit = {}
    override def onResourceManagerReload(resourceManager:IResourceManager):Unit = {}
}

class FramedWireModelLoader extends IModelLoader[FramedWireModelLoader] with IModelGeometry[FramedWireModelLoader] {
    override def read(deserializationContext: JsonDeserializationContext, modelContents: JsonObject) = this

    override def getTextures(owner: IModelConfiguration, modelGetter: JFunc[ResourceLocation, IUnbakedModel], missingTextureErrors: util.Set[Pair[String, String]]) = Collections.emptyList()

    override def bake(owner: IModelConfiguration, bakery: ModelBakery, spriteGetter: JFunc[Material, TextureAtlasSprite], modelTransform: IModelTransform, overrides: ItemOverrideList, modelLocation: ResourceLocation) = FramedWireItemRenderer

    // Following 2 methods included because of compile issues
    override def onResourceManagerReload(resourceManager:IResourceManager, resourcePredicate:Predicate[IResourceType]):Unit = {}
    override def onResourceManagerReload(resourceManager:IResourceManager):Unit = {}
}


