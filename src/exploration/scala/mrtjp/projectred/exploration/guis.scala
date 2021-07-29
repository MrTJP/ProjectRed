package mrtjp.projectred.exploration

import codechicken.lib.colour.EnumColour
import codechicken.lib.texture.TextureUtils
import com.mojang.blaze3d.matrix.MatrixStack
import mrtjp.core.gui.NodeGui
import mrtjp.core.vec.Point
import mrtjp.projectred.ProjectRedExploration
import net.minecraft.entity.player.PlayerInventory
import net.minecraft.util.ResourceLocation
import net.minecraft.util.text.ITextComponent

class GuiBackpack(container: ContainerBackpack, playerInv: PlayerInventory, title: ITextComponent) extends NodeGui(container, 176, 168, playerInv, title)
{
    override def drawBack_Impl(stack:MatrixStack, mouse:Point, frame:Float)
    {
        TextureUtils.changeTexture(new ResourceLocation(ProjectRedExploration.MOD_ID, "textures/gui/backpack.png"))
        blit(stack, 0, 0, 0, 0, getXSize, getYSize)
    }

    override def drawFront_Impl(stack:MatrixStack, mouse:Point, frame:Float)
    {
        getFontRenderer.draw(stack, title, 8, 6, EnumColour.GRAY.argb)
        getFontRenderer.draw(stack, playerInv.getName, 8, 75, EnumColour.GRAY.argb)
    }

    override def keyPressed_Impl(c:Char, keycode:Int, consumed:Boolean) =
    {
        keycode == playerInv.player.inventory.selected + 2
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
