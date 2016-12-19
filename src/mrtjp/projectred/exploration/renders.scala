package mrtjp.projectred.exploration

import java.util

import codechicken.lib.render._
import codechicken.lib.render.item.{IItemRenderer, IMatrixTransform}
import codechicken.lib.texture.TextureUtils
import codechicken.lib.util.TransformUtils
import codechicken.lib.vec._
import codechicken.lib.vec.uv.UVTranslation
import net.minecraft.block.state.IBlockState
import net.minecraft.client.renderer.block.model.{BakedQuad, ItemCameraTransforms, ItemOverrideList}
import net.minecraft.client.renderer.block.model.ItemCameraTransforms.TransformType
import net.minecraft.client.renderer.texture.TextureAtlasSprite
import net.minecraft.client.renderer.vertex.DefaultVertexFormats
import net.minecraft.item.Item.ToolMaterial
import net.minecraft.item.ItemStack
import net.minecraft.util.{EnumFacing, ResourceLocation}
import net.minecraftforge.client.model.IPerspectiveAwareModel
import net.minecraftforge.client.model.IPerspectiveAwareModel.MapWrapper
import net.minecraftforge.common.model.TRSRTransformation
import org.lwjgl.opengl.GL11

import scala.collection.JavaConversions._

object GemSawRenderer extends IItemRenderer with IPerspectiveAwareModel
{
    private val models = CCOBJParser.parseObjModels(new ResourceLocation("microblock", "models/saw.obj"), 7, new SwapYZ)
    private val handle = models.get("Handle")
    private val holder = models.get("BladeSupport")
    private val blade = models.get("Blade")

    import mrtjp.projectred.ProjectRedExploration.{toolMaterialPeridot, toolMaterialRuby, toolMaterialSapphire}
    import codechicken.lib.colour.EnumColour._
    private def colour(stack:ItemStack) = stack.getItem.asInstanceOf[ItemGemSaw].toolDef.mat match
    {
        case ToolMaterial.WOOD => BROWN.rgba
        case ToolMaterial.STONE => LIGHT_GRAY.rgba
        case ToolMaterial.IRON => WHITE.rgba
        case ToolMaterial.GOLD => YELLOW.rgba
        case t if t == toolMaterialRuby => RED.rgba
        case t if t == toolMaterialSapphire => BLUE.rgba
        case t if t == toolMaterialPeridot => GREEN.rgba
        case ToolMaterial.DIAMOND => CYAN.rgba
        case _ => BLACK.rgba
    }

    override def renderItem(item:ItemStack)
    {
        val ccrs = CCRenderState.instance
        ccrs.reset()
        ccrs.pullLightmap()
        TextureUtils.changeTexture("microblock:textures/items/saw.png")
        ccrs.baseColour = 0xFFFFFFFF

        ccrs.startDrawing(7, DefaultVertexFormats.ITEM)
        handle.render(ccrs)
        holder.render(ccrs)
        ccrs.draw()

        //if (rtype != ItemRenderType.EQUIPPED_FIRST_PERSON) GL11.glDisable(GL11.GL_LIGHTING)
        GL11.glDisable(GL11.GL_CULL_FACE)

        ccrs.startDrawing(7, DefaultVertexFormats.ITEM)
        ccrs.baseColour = colour(item)
        blade.render(ccrs, new UVTranslation(0, 4/64D))
        ccrs.baseColour = 0xFFFFFFFF
        ccrs.draw()

        GL11.glEnable(GL11.GL_CULL_FACE)
        //if (rtype != EQUIPPED_FIRST_PERSON) GL11.glEnable(GL11.GL_LIGHTING)
    }

    override def handlePerspective(cameraTransformType: TransformType) = MapWrapper.handlePerspective(this, TRSRTransformation.identity(), cameraTransformType)

    /*override def getTransform(transformType: TransformType, isLeftHand: Boolean): Matrix4 = {//TODO
        import codechicken.lib.vec.{TransformationList => TList}
        import net.minecraft.client.renderer.block.model.ItemCameraTransforms.TransformType._

        val temp = new Matrix4
        temp.m00 = -1
        val flipx = new Matrix4().multiply(temp)

        def flipX(matrix:Matrix4): Matrix4 = {
            Matrix4.blockCenterToBlockCorner(flipx.copy().multiply(Matrix4.blockCornerToBlockCenter(matrix)).multiply(flipx))
        }
        new Matrix4()
        /*transformType match
        {
            case GUI => new Matrix4()(new TList(new Scale(1.8), new Translation(0, 0, -0.6), new Rotation(-MathHelper.pi/4, 1, 0, 0), new Rotation(MathHelper.pi*3/4, 0, 1, 0)))
            case GROUND => new Matrix4()(new TList(new Scale(1), new Translation(0, 0, -0.25), new Rotation(-MathHelper.pi/4, 1, 0, 0)))
            case FIRST_PERSON_RIGHT_HAND => new Matrix4()(new TList(new Scale(1.5), new Rotation(-MathHelper.pi/3, 1, 0, 0), new Rotation(MathHelper.pi*3/4, 0, 1, 0), new Translation(0.5, 0.5, 0.5)))
            case THIRD_PERSON_RIGHT_HAND => new Matrix4()(new TList(new Scale(1.5), new Rotation(-MathHelper.pi/5, 1, 0, 0), new Rotation(-MathHelper.pi*3/4, 0, 1, 0), new Translation(0.75, 0.5, 0.75)))
            case FIRST_PERSON_LEFT_HAND => flipX(new Matrix4()(new TList(new Scale(1.5), new Rotation(-MathHelper.pi/3, 1, 0, 0), new Rotation(MathHelper.pi*3/4, 0, 1, 0), new Translation(0.5, 0.5, 0.5))))
            case THIRD_PERSON_LEFT_HAND => flipX(new Matrix4()(new TList(new Scale(1.5), new Rotation(-MathHelper.pi/5, 1, 0, 0), new Rotation(-MathHelper.pi*3/4, 0, 1, 0), new Translation(0.75, 0.5, 0.75))))
        }*/
    }*/

    override def getQuads(state: IBlockState, side: EnumFacing, rand: Long): util.List[BakedQuad] = new util.ArrayList[BakedQuad]()

    override def isAmbientOcclusion: Boolean = true

    override def isGui3d: Boolean = true

    override def isBuiltInRenderer: Boolean = true

    override def getParticleTexture: TextureAtlasSprite = null

    override def getItemCameraTransforms: ItemCameraTransforms = ItemCameraTransforms.DEFAULT

    override def getOverrides: ItemOverrideList = ItemOverrideList.NONE
}

//object RenderLily extends TInstancedBlockRender
//{
//    val model =
//    {
//        val m = CCModel.quadModel(8)
//        m.generateBlock(0, new Cuboid6(0, 0, 0.5, 1, 1, 0.5), ~0xC)
//        val m2 = m.copy.apply(Rotation.quarterRotations(1).at(Vector3.center))
//        val m3 = CCModel.combine(Seq(m, m2))
//        m3.apply(new Rotation(45*MathHelper.torad, 0, 1, 0).at(Vector3.center))
//        m3.computeNormals()
//        m3.shrinkUVs(0.0001)
//        m3.computeLighting(LightModel.standardLightModel)
//    }
//
//    val icons = new Array[IIcon](8)
//    var iconC:IIcon = null
//
//    override def renderWorldBlock(r:RenderBlocks, w:IBlockAccess, x:Int, y:Int, z:Int, meta:Int) =
//    {
//        val te = WorldLib.getTileEntity(w, x, y, z, classOf[TileLily])
//        if (te != null)
//        {
//            CCRenderState.reset()
//            CCRenderState.lightMatrix.locate(w, x, y, z)
//            CCRenderState.setBrightness(w, x, y, z)
//            model.render(new Translation(x, y, z), new IconTransformation(icons(te.growth)))
//            if (te.growth == 7)
//                model.render(new Translation(x, y, z), new IconTransformation(iconC), new ColourMultiplier(Colors(te.meta).rgba))
//        }
//    }
//
//    override def getIcon(side:Int, meta:Int) = icons(7)
//
//    override def renderInvBlock(r:RenderBlocks, meta:Int)
//    {
//        CCRenderState.reset()
//        CCRenderState.setDynamic()
//        CCRenderState.pullLightmap()
//        CCRenderState.setNormal(0, -1, 0)
//        CCRenderState.startDrawing()
//        r.drawCrossedSquares(icons(7), -0.5D, -0.95D, -0.5D, 1.65f)
//        CCRenderState.draw()
//    }
//
//    override def registerIcons(reg:IIconRegister)
//    {
//        for (i <- 0 until 8) icons(i) = reg.registerIcon("projectred:world/lily/lily_"+i)
//        iconC = reg.registerIcon("projectred:world/lily/lily_colour")
//    }
//}
