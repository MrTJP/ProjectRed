package mrtjp.projectred.renderstuffs;

import mrtjp.projectred.blocks.BlockLamp;
import mrtjp.projectred.tiles.TileLamp;
import net.minecraft.block.Block;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.client.renderer.tileentity.TileEntitySpecialRenderer;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.client.IItemRenderer;

import org.lwjgl.opengl.GL11;

import cpw.mods.fml.client.FMLClientHandler;
import cpw.mods.fml.client.registry.ISimpleBlockRenderingHandler;

public class LampRenderer extends TileEntitySpecialRenderer {
	public static final LampRenderer instance = new LampRenderer();
	private LampModel model = new LampModel();

	@Override
	public void renderTileEntityAt(TileEntity tileentity, double x, double y, double z, float f) {
		if (tileentity instanceof TileLamp) {
			TileLamp lamp = (TileLamp) tileentity;
			if (lamp.getLightValue() == 15) {
				model.renderLampShade(x, y, z, lamp.getColor(), lamp.random);
			}
		}
	}
}
