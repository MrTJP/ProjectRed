package mrtjp.projectred.exploration

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.MCDataInput
import codechicken.lib.texture.TextureUtils
import codechicken.lib.util.ItemUtils
import mrtjp.core.gui.{NodeGui, TGuiFactory}
import mrtjp.core.vec.Point
import mrtjp.projectred.ProjectRedExploration
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.util.ResourceLocation
import net.minecraftforge.fml.relauncher.{Side, SideOnly}
import org.lwjgl.opengl.GL11

class GuiBackpack(player:EntityPlayer, bag:ItemStack) extends NodeGui(ItemBackpack.createContainer(player), 176, 168)
{
    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F)
        TextureUtils.changeTexture(new ResourceLocation("projectred", "textures/gui/backpack.png"))
        drawTexturedModalRect(0, 0, 0, 0, xSize, ySize)
    }

    override def drawFront_Impl(mouse:Point, frame:Float)
    {
        fontRenderer.drawString(bag.getDisplayName, 8, 6, EnumColour.GRAY.rgb)
        fontRenderer.drawString("Inventory", 8, 75, EnumColour.GRAY.rgb)
    }

    override def keyPressed_Impl(c:Char, keycode:Int, consumed:Boolean) =
    {
        keycode == player.inventory.currentItem+2
    }
}

object GuiBackpack extends TGuiFactory
{
    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        val held = ItemUtils.getHeldStack(player)
            if (!held.isEmpty && held.getItem == ProjectRedExploration.itemBackpack)
                new GuiBackpack(player, held)
        else null
    }

    override def getID = ExplorationProxy.guiIDBackpack
}
