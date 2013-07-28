package mrtjp.projectred.renderstuffs;

import mrtjp.projectred.crafting.AlloySmelterRecipe;
import mrtjp.projectred.tiles.TileTurbineRotary;
import mrtjp.projectred.utils.BasicGuiUtils;
import mrtjp.projectred.utils.BasicGuiUtils.GuiItemRenderOptions;
import mrtjp.projectred.utils.gui.BaseGuiContainer;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ResourceLocation;

import org.lwjgl.opengl.GL11;

public class GuiTurbineRotary extends BaseGuiContainer {

	TileTurbineRotary tile;

	public GuiTurbineRotary(EntityPlayer player, TileTurbineRotary tile) {
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
		mc.renderEngine.func_110577_a(new ResourceLocation("projectred", "textures/gui/turbinerotary.png"));
		int j = guiLeft;
		int k = guiTop;
		drawTexturedModalRect(j, k, 0, 0, xSize, ySize);

		mc.renderEngine.func_110577_a(new ResourceLocation("projectred", "textures/gui/turbinerotary.png"));
		GL11.glDisable(2929 /* GL_DEPTH_TEST */);
		drawRect(guiLeft + 141, guiTop + 47, guiLeft + 157, guiTop + 63, 0xc08b8b8b);
		GL11.glEnable(2929 /* GL_DEPTH_TEST */);
		GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F);
	}

}
