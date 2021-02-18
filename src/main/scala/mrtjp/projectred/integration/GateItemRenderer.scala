package mrtjp.projectred.integration

import codechicken.lib.render.CCRenderState
import codechicken.lib.render.item.IItemRenderer
import codechicken.lib.util.TransformUtils
import codechicken.lib.vec.RedundantTransformation
import com.google.gson.{JsonDeserializationContext, JsonObject}
import com.mojang.blaze3d.matrix.MatrixStack
import com.mojang.datafixers.util.Pair
import net.minecraft.client.renderer.model._
import net.minecraft.client.renderer.texture.TextureAtlasSprite
import net.minecraft.client.renderer.{IRenderTypeBuffer, RenderType}
import net.minecraft.item.ItemStack
import net.minecraft.util.ResourceLocation
import net.minecraftforge.client.model.geometry.IModelGeometry
import net.minecraftforge.client.model.{IModelConfiguration, IModelLoader}

import java.util
import java.util.Collections
import java.util.function.{Function => JFunc}

object GateItemRenderer extends IItemRenderer
{
    override def isAmbientOcclusion = true

    override def isGui3d = true

    override def getTransforms = TransformUtils.DEFAULT_BLOCK

    override def func_230044_c_() = true

    override def renderItem(stack:ItemStack, transformType:ItemCameraTransforms.TransformType, mStack:MatrixStack, getter:IRenderTypeBuffer, packedLight:Int, packedOverlay:Int):Unit =
    {
        stack.getItem match {
            case gate:ItemPartGate =>
                val ccrs = CCRenderState.instance()
                ccrs.reset()
                ccrs.brightness = packedLight
                ccrs.overlay = packedOverlay
                ccrs.bind(RenderType.getCutout, getter, mStack)
                RenderGate.renderInv(stack, new RedundantTransformation,gate.gateType, ccrs)
        }
    }

    //    override def renderItem(item:ItemStack, transformType: TransformType)
    //    {
    //        val meta = item.getItemDamage
    //        if (!GateDefinition.values.isDefinedAt(meta) ||
    //                !GateDefinition(meta).implemented) return
    //
    //        enableBlend()
    //        blendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA)
    //
    //        TextureUtils.bindBlockTexture()
    //        val ccrs = CCRenderState.instance()
    //        ccrs.reset()
    //        ccrs.pullLightmap()
    //        RenderGate.renderInv(item, new RedundantTransformation, item.getItemDamage, ccrs)
    //
    //        disableBlend()
    //    }

    class Loader extends IModelLoader[Loader] with IModelGeometry[Loader] {
        override def read(deserializationContext: JsonDeserializationContext, modelContents: JsonObject) = this

        override def getTextures(owner: IModelConfiguration, modelGetter: JFunc[ResourceLocation, IUnbakedModel], missingTextureErrors: util.Set[Pair[String, String]]) = Collections.emptyList()

        override def bake(owner: IModelConfiguration, bakery: ModelBakery, spriteGetter: JFunc[Material, TextureAtlasSprite], modelTransform: IModelTransform, overrides: ItemOverrideList, modelLocation: ResourceLocation) = GateItemRenderer
    }
}
