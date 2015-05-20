package mrtjp.projectred.exploration

import codechicken.lib.data.MCDataInput
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.color.Colors
import mrtjp.core.gui.{NodeGui, TGuiBuilder}
import mrtjp.core.vec.Point
import mrtjp.projectred.ProjectRedExploration
import mrtjp.projectred.core.libmc.PRResources
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import org.lwjgl.opengl.GL11

class GuiBackpack(player:EntityPlayer, bag:ItemStack) extends NodeGui(ItemBackpack.createContainer(player), 176, 168)
{
    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F)
        PRResources.guiBag.bind()
        drawTexturedModalRect(0, 0, 0, 0, xSize, ySize)
    }

    override def drawFront_Impl(mouse:Point, frame:Float)
    {
        fontRenderer.drawString(bag.getDisplayName, 8, 6, Colors.GREY.rgb)
        fontRenderer.drawString("Inventory", 8, 75, Colors.GREY.rgb)
    }

    override def keyPressed_Impl(c:Char, keycode:Int, consumed:Boolean) =
    {
        keycode == player.inventory.currentItem+2
    }
}

object GuiBackpack extends TGuiBuilder
{
    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        val held = player.getHeldItem
            if (held != null && held.getItem == ProjectRedExploration.itemBackpack)
                new GuiBackpack(player, held)
        else null
    }

    override def getID = ExplorationProxy.guiIDBackpack
}
