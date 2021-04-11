package mrtjp.projectred.exploration

import codechicken.lib.colour.EnumColour
import codechicken.lib.texture.TextureUtils
import mrtjp.core.gui.NodeGui
import mrtjp.core.vec.Point
import mrtjp.projectred.ProjectRedExploration
import net.minecraft.entity.player.PlayerInventory
import net.minecraft.util.ResourceLocation
import net.minecraft.util.text.ITextComponent

class GuiBackpack(container: ContainerBackpack, playerInv: PlayerInventory, title: ITextComponent) extends NodeGui(container, 176, 168, playerInv, title)
{
    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        TextureUtils.changeTexture(new ResourceLocation(ProjectRedExploration.MOD_ID, "textures/gui/backpack.png"))
        blit(0, 0, 0, 0, xSize, ySize)
    }

    override def drawFront_Impl(mouse:Point, frame:Float)
    {
        getFontRenderer.drawString(title.getFormattedText, 8, 6, EnumColour.GRAY.rgb)
        getFontRenderer.drawString(playerInventory.getName.getFormattedText, 8, 75, EnumColour.GRAY.rgb)
    }

    override def keyPressed_Impl(c:Char, keycode:Int, consumed:Boolean) =
    {
        keycode == playerInventory.player.inventory.currentItem + 2
    }
}

//object GuiBackpack extends TGuiFactory
//{
//    @SideOnly(Side.CLIENT)
//    override def buildGui(player:EntityPlayer, data:MCDataInput) =
//    {
//        val held = ItemUtils.getHeldStack(player)
//            if (!held.isEmpty && held.getItem == ProjectRedExploration.itemBackpack)
//                new GuiBackpack(player, held)
//        else null
//    }
//
//    override def getID = ExplorationProxy.guiIDBackpack
//}
