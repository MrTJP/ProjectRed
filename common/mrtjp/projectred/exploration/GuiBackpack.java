package mrtjp.projectred.exploration;

import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.core.inventory.SpecialGuiContainer;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.IInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ResourceLocation;

import org.lwjgl.opengl.GL11;

public class GuiBackpack extends SpecialGuiContainer
{
    ItemStack _bag;
    EntityPlayer _player;

    public GuiBackpack(EntityPlayer player, IInventory backpack, ItemStack bag)
    {
        super(ItemBackpack.getContainer(player), null, 176, 168);
        _bag = bag;
        _player = player;
    }

    @Override
    public void drawBackground()
    {
        GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F);
        mc.renderEngine.bindTexture(new ResourceLocation("projectred", "textures/gui/bpgui.png"));

        drawTexturedModalRect(0, 0, 0, 0, xSize, ySize);
    }

    @Override
    public void drawForeground()
    {
        fontRenderer.drawString("Backpack", 8, 6, PRColors.GREY.rgb);
        fontRenderer.drawString("Inventory", 8, 75, PRColors.GREY.rgb);
    }

    @Override
    public void keyTyped(char par1, int id)
    {
        if (id >= 2 && id <= 10)
        {
            if (id-1 == _player.inventory.currentItem+1)
                return;
            else
                super.keyTyped(par1, id);
        }
        else
            super.keyTyped(par1, id);
    }
}
