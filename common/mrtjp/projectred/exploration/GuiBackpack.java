package mrtjp.projectred.exploration;

import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.core.BaseGuiContainer;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.IInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ResourceLocation;

import org.lwjgl.opengl.GL11;

public class GuiBackpack extends BaseGuiContainer {
    ItemStack _bag;
    EntityPlayer _player;

    public GuiBackpack(EntityPlayer player, IInventory backpack, ItemStack bag) {
        super(176, 168, 0, 0);
        _bag = bag;
        _player = player;
        if (_bag.itemID == ProjectRedExploration.itemBackpack.itemID)
            inventorySlots = ItemBackpack.getContainer(player);
    }

    @Override
    protected void drawGuiContainerForegroundLayer(int par1, int par2) {
        super.drawGuiContainerForegroundLayer(par1, par2);
        this.fontRenderer.drawString("Backpack", 8, 6, 4210752);
    }

    @Override
    protected void drawGuiContainerBackgroundLayer(float var1, int var2, int var3) {
        GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F);
        mc.renderEngine.bindTexture(new ResourceLocation("projectred", "textures/gui/bpgui.png"));
        int j = guiLeft;
        int k = guiTop;
        drawTexturedModalRect(j, k, 0, 0, xSize, ySize);
    }

    @Override
    protected void keyTyped(char par1, int id) {
        if (id >= 2 && id <= 10) {
            int actualKeyboardButton = id - 1;
            if (actualKeyboardButton == _player.inventory.currentItem + 1)
                return;
            else
                super.keyTyped(par1, id);
        } else
            super.keyTyped(par1, id);
    }
}
