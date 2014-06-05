package mrtjp.projectred.exploration

import mrtjp.projectred.core.{GuiIDs, TGuiBuilder}
import codechicken.lib.data.MCDataInput
import net.minecraft.entity.player.EntityPlayer
import mrtjp.projectred.core.libmc.gui.{Point, WidgetGui}
import net.minecraft.item.ItemStack
import mrtjp.projectred.ProjectRedExploration
import org.lwjgl.opengl.GL11
import mrtjp.projectred.core.libmc.{PRColors, ResourceLib}

class GuiBackpack(player:EntityPlayer, bag:ItemStack) extends WidgetGui(ItemBackpack.createContainer(player), 176, 168)
{
    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F)
        ResourceLib.guiBag.bind()
        drawTexturedModalRect(0, 0, 0, 0, xSize, ySize)
    }

    override def drawFront_Impl(mouse:Point, frame:Float)
    {
        fontRenderer.drawString(bag.getDisplayName, 8, 6, PRColors.GREY.rgb)
        fontRenderer.drawString("Inventory", 8, 75, PRColors.GREY.rgb)
    }

    override def blockedHotkeyNumbers = Set(player.inventory.currentItem+1)
}

object GuiBackpack extends TGuiBuilder
{
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        val held = player.getHeldItem
            if (held != null && held.getItem == ProjectRedExploration.itemBackpack)
                new GuiBackpack(player, held)
        else null
    }

    override def getID = GuiIDs.backpacks
}
