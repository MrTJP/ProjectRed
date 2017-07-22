package mrtjp.projectred.transmission

import codechicken.lib.render._
import codechicken.lib.render.item.IItemRenderer
import codechicken.lib.render.pipeline.IVertexOperation
import codechicken.lib.texture.TextureUtils
import codechicken.lib.util.TransformUtils
import codechicken.lib.vec.uv.IconTransformation
import codechicken.lib.vec.{Scale, Translation, Vector3}
import com.google.common.collect.ImmutableList
import net.minecraft.block.state.IBlockState
import net.minecraft.client.renderer.block.model.ItemCameraTransforms.TransformType
import net.minecraft.client.renderer.block.model.{ItemCameraTransforms, ItemOverrideList}
import net.minecraft.client.renderer.vertex.DefaultVertexFormats
import net.minecraft.item.ItemStack
import net.minecraft.util.EnumFacing
import net.minecraftforge.client.model.IPerspectiveAwareModel
import net.minecraftforge.client.model.IPerspectiveAwareModel.MapWrapper
import net.minecraftforge.common.model.IModelState
import org.lwjgl.opengl.GL11

trait TWireItemRenderCommon extends IItemRenderer with IPerspectiveAwareModel
{
    override def isAmbientOcclusion = true
    override def isGui3d = true
    override def getTransforms = TransformUtils.DEFAULT_BLOCK

    override def renderItem(item:ItemStack, transformType: TransformType)
    {
        renderWireInventory(item.getItemDamage, 0, 0, 0, 1)
    }

    def renderWireInventory(meta:Int, x:Float, y:Float, z:Float, scale:Float)
    {
        val wdef = WireDef.values(meta)
        if (wdef == null) return

        val ccrs = CCRenderState.instance()
        TextureUtils.bindBlockTexture()
        ccrs.reset()
        ccrs.pullLightmap()
        ccrs.startDrawing(GL11.GL_QUADS, DefaultVertexFormats.ITEM)

        doRender(wdef.thickness, wdef.itemColour<<8|0xFF, ccrs, new Scale(scale).at(Vector3.center).`with`(new Translation(x, y, z)),
            new IconTransformation(wdef.wireSprites(0)))

        ccrs.draw()
    }

    def doRender(thickness:Int, renderHue:Int, ccrs:CCRenderState, ops:IVertexOperation*)
}

object WireItemRenderer extends TWireItemRenderCommon
{
//    override def renderItem(rtype:ItemRenderType, item:ItemStack, data:AnyRef*)
//    {
//        val damage = item.getItemDamage
//        import ItemRenderType._
//        rtype match
//        {
//            case ENTITY => renderWireInventory(damage, -0.5f, 0f, -0.5f, 0.6f)
//            case EQUIPPED => renderWireInventory(damage, 0f, 0.45f, 0f, 1f)
//            case EQUIPPED_FIRST_PERSON => renderWireInventory(damage, 0.0f, 0.5f, 0.0f, 1f)
//            case INVENTORY => renderWireInventory(damage, -0.5f, -0.2f, -0.5f, 1f)
//            case _ =>
//        }
//    }

    override def doRender(thickness:Int, renderHue:Int, ccrs:CCRenderState, ops:IVertexOperation*)
    {
        RenderWire.renderInv(thickness, renderHue, ccrs, ops:_*)
    }
}

object FramedWireItemRenderer extends TWireItemRenderCommon
{
//    override def renderItem(rtype:ItemRenderType, item:ItemStack, data:AnyRef*)
//    {
//        val damage = item.getItemDamage
//        import ItemRenderType._
//        rtype match
//        {
//            case ENTITY => renderWireInventory(damage, -0.5f, 0f, -0.5f, 1f)
//            case EQUIPPED => renderWireInventory(damage, 0f, 0f, 0f, 1f)
//            case EQUIPPED_FIRST_PERSON => renderWireInventory(damage, 0f, 0f, 0f, 1f)
//            case INVENTORY => renderWireInventory(damage, -0.5f, -0.5f, -0.5f, 1f)
//            case _ =>
//        }
//    }

    override def doRender(thickness:Int, renderHue:Int, ccrs:CCRenderState, ops:IVertexOperation*)
    {
        RenderFramedWire.renderInv(thickness, renderHue, ccrs, ops:_*)
    }
}
