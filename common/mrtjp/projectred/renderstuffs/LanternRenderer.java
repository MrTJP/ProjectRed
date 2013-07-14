package mrtjp.projectred.renderstuffs;

import mrtjp.projectred.blocks.BlockLantern.EnumLantern;
import mrtjp.projectred.core.ProjectRedTickHandler;
import mrtjp.projectred.tiles.TileLantern;
import mrtjp.projectred.utils.Color;
import mrtjp.projectred.utils.codechicken.core.render.CCRenderState;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.tileentity.TileEntitySpecialRenderer;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntity;
import net.minecraftforge.client.IItemRenderer;
import net.minecraftforge.common.ForgeDirection;

import org.lwjgl.opengl.GL11;

import codechicken.translocator.TileTranslocatorRenderer;

public class LanternRenderer extends TileEntitySpecialRenderer implements IItemRenderer {
	public static LanternRenderer instance = new LanternRenderer();
	private LanternModel model = new LanternModel();

	@Override
	public void renderTileEntityAt(TileEntity te, double x, double y, double z, float f) {
		if (te != null && te instanceof TileLantern) {
			TileLantern tile = (TileLantern) te;
			int color = tile.lanternmeta > 15 ? tile.lanternmeta - 16 : tile.lanternmeta;
			boolean isOn = tile.getLightValue() == 15;
			int rotation = tile.rotation;
			CCRenderState.setBrightness(te.worldObj, (int)x, (int)y, (int)z);
			// Bind the texture
			model.bindTextureForColorAndState(color, isOn);
			
			// Render the core
			model.renderLampBulb(x, y, z, rotation, isOn);
			
			// Render halo
			if (isOn) {
				model.renderLampShade(x, y, z, color);
			}
		}
	}

	@Override
	public boolean handleRenderType(ItemStack item, ItemRenderType type) {
		return true;
	}

	@Override
	public boolean shouldUseRenderHelper(ItemRenderType type, ItemStack item, ItemRendererHelper helper) {
		return true;
	}

	@Override
	public void renderItem(ItemRenderType type, ItemStack item, Object... data) {
		int color = item.getItemDamage() > 15 ? item.getItemDamage() - 16 : item.getItemDamage();
		boolean isOn = item.getItemDamage() > 15;
		model.bindTextureForColorAndState(color, isOn);
		
		switch (type) {
		case ENTITY:
			model.renderInventory(0f, 0f, 0f, 1f);
			return;
		case EQUIPPED:
			model.renderInventory(1f, 0f, 0f, 1f);
			return;
		case EQUIPPED_FIRST_PERSON:
			model.renderInventory(1f, 0f, 0f, 1f);
			return;
		case INVENTORY:
			model.renderInventory(3f, .6f, 1f, 2f);
			return;
		default: return;
		}
		
	}
}
