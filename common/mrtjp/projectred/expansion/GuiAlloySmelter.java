package mrtjp.projectred.expansion;

import mrtjp.projectred.core.BaseGuiContainer;
import mrtjp.projectred.core.BasicGuiUtils;
import mrtjp.projectred.core.BasicGuiUtils.GuiItemRenderOptions;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ResourceLocation;

import org.lwjgl.opengl.GL11;

public class GuiAlloySmelter extends BaseGuiContainer {

    TileAlloySmelter tile;

    public GuiAlloySmelter(EntityPlayer player, TileAlloySmelter tile) {
        super(176, 166, 0, 0);
        this.inventorySlots = tile.getContainer(player);
        this.tile = tile;
    }

    @Override
    protected void drawGuiContainerForegroundLayer(int par1, int par2) {
        super.drawGuiContainerForegroundLayer(par1, par2);
    }

    @Override
    protected void drawGuiContainerBackgroundLayer(float var1, int var2, int var3) {
        GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F);
        mc.renderEngine.func_110577_a(new ResourceLocation("projectred", "textures/gui/alloysmelter.png"));
        int j = guiLeft;
        int k = guiTop;
        drawTexturedModalRect(j, k, 0, 0, xSize, ySize);

        mc.renderEngine.func_110577_a(new ResourceLocation("projectred", "textures/gui/alloysmelter.png"));
        GL11.glDisable(2929 /* GL_DEPTH_TEST */);
        drawRect(guiLeft + 141, guiTop + 47, guiLeft + 157, guiTop + 63, 0xc08b8b8b);
        GL11.glEnable(2929 /* GL_DEPTH_TEST */);
        GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F);

        // Draw heat level, level 0 is full flame
        int level = (int) Math.ceil(1600 - tile.heat);
        drawTexturedModalRect(j + 131, k + 19 + (level * 14 / 1600), 176, level * 14 / 1600, 14, 14 - (level * 14 / 1600));

        // Draw progress indicator
        if (tile.hasWork) {
            AlloySmelterRecipe r = tile.getSuggestedRecipe();
            if (r == null) {
                return;
            }
            int progress = 100 * tile.progress / r.getBurnTime();
            if (progress >= 50) {
                drawTexturedModalRect(j + 107, k + 38, 176, 14, 10, 24);
                drawTexturedModalRect(j + 117, k + 38, 186, 14, ((progress - 50) * 26 / 100), 24);
            } else {
                if (progress >= 25) {
                    drawTexturedModalRect(j + 107, k + 38, 176, 14, ((progress - 25) * 10 / 25), 24);
                }
                drawTexturedModalRect(j + 114, k + 38, 183, 14, 3, (progress * 17 / 50));
            }
            
            ItemStack resultstack = r.getResult().copy();
            if (resultstack != null) {
                BasicGuiUtils.renderItemOnGui(new GuiItemRenderOptions(resultstack).setPos(guiLeft + 141, guiTop + 47).setPulsate(.15f, .55f));
            }
        }
    }

}
