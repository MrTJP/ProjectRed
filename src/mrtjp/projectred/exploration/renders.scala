package mrtjp.projectred.exploration

import mrtjp.core.color.Colors
import net.minecraftforge.client.IItemRenderer
import net.minecraftforge.client.IItemRenderer.{ItemRendererHelper, ItemRenderType}
import net.minecraft.item.ItemStack
import net.minecraft.item.Item.ToolMaterial
import mrtjp.projectred.ProjectRedExploration
import codechicken.lib.vec.{SwapYZ, Rotation, Translation, Scale}
import codechicken.lib.math.MathHelper
import codechicken.lib.render.{CCModel, CCRenderState}
import org.lwjgl.opengl.GL11
import net.minecraft.util.ResourceLocation
import codechicken.lib.render.uv.UVTranslation

object GemSawRenderer extends IItemRenderer
{
    private val models = CCModel.parseObjModels(new ResourceLocation("microblock", "models/saw.obj"), 7, new SwapYZ)
    private val handle = models.get("Handle")
    private val holder = models.get("BladeSupport")
    private val blade = models.get("Blade")

    override def handleRenderType(item:ItemStack, t:ItemRenderType) = true

    override def shouldUseRenderHelper(t:ItemRenderType, item:ItemStack, helper:ItemRendererHelper) = true

    import Colors._
    import ProjectRedExploration.{toolMaterialPeridot, toolMaterialRuby, toolMaterialSapphire}
    private def colour(stack:ItemStack) = stack.getItem.asInstanceOf[ItemGemSaw].tool.mat match
    {
        case ToolMaterial.WOOD => BROWN.rgba
        case ToolMaterial.STONE => LIGHT_GREY.rgba
        case ToolMaterial.IRON => WHITE.rgba
        case ToolMaterial.GOLD => YELLOW.rgba
        case t if t == toolMaterialRuby => RED.rgba
        case t if t == toolMaterialSapphire => BLUE.rgba
        case t if t == toolMaterialPeridot => GREEN.rgba
        case ToolMaterial.EMERALD => CYAN.rgba
        case _ => BLACK.rgba
    }

    override def renderItem(rtype:ItemRenderType, item:ItemStack, data:AnyRef*)
    {
        import ItemRenderType._
        import codechicken.lib.vec.{TransformationList => TList}
        val t = rtype match
        {
            case INVENTORY => new TList(new Scale(1.8), new Translation(0, 0, -0.6), new Rotation(-MathHelper.pi/4, 1, 0, 0), new Rotation(MathHelper.pi*3/4, 0, 1, 0))
            case ENTITY => new TList(new Scale(1), new Translation(0, 0, -0.25), new Rotation(-MathHelper.pi/4, 1, 0, 0))
            case EQUIPPED_FIRST_PERSON => new TList(new Scale(1.5), new Rotation(-MathHelper.pi/3, 1, 0, 0), new Rotation(MathHelper.pi*3/4, 0, 1, 0), new Translation(0.5, 0.5, 0.5))
            case EQUIPPED => new TList(new Scale(1.5), new Rotation(-MathHelper.pi/5, 1, 0, 0), new Rotation(-MathHelper.pi*3/4, 0, 1, 0), new Translation(0.75, 0.5, 0.75))
            case _ => return
        }

        CCRenderState.reset()
        CCRenderState.setDynamic()
        CCRenderState.pullLightmap()
        CCRenderState.changeTexture("microblock:textures/items/saw.png")
        CCRenderState.baseColour = 0xFFFFFFFF

        CCRenderState.startDrawing()
        handle.render(t)
        holder.render(t)
        CCRenderState.draw()

        if (rtype != ItemRenderType.EQUIPPED_FIRST_PERSON) GL11.glDisable(GL11.GL_LIGHTING)
        GL11.glDisable(GL11.GL_CULL_FACE)

        CCRenderState.startDrawing()
        CCRenderState.baseColour = colour(item)
        blade.render(t, new UVTranslation(0, 4/64D))
        CCRenderState.baseColour = 0xFFFFFFFF
        CCRenderState.draw()

        GL11.glEnable(GL11.GL_CULL_FACE)
        if (rtype != EQUIPPED_FIRST_PERSON) GL11.glEnable(GL11.GL_LIGHTING)
    }
}