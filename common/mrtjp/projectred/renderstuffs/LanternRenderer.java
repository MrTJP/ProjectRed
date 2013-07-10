package mrtjp.projectred.renderstuffs;

import org.lwjgl.opengl.GL11;

import mrtjp.projectred.blocks.BlockLantern.EnumLantern;
import mrtjp.projectred.tiles.TileLantern;
import mrtjp.projectred.utils.BasicUtils;
import mrtjp.projectred.utils.Color;
import mrtjp.projectred.utils.Coords;
import net.minecraft.block.Block;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.client.renderer.tileentity.TileEntitySpecialRenderer;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.world.IBlockAccess;
import cpw.mods.fml.client.registry.ISimpleBlockRenderingHandler;

public class LanternRenderer extends TileEntitySpecialRenderer implements ISimpleBlockRenderingHandler {
	public static LanternRenderer instance = new LanternRenderer();
	private LanternModel model = new LanternModel();

	@Override
	public void renderInventoryBlock(Block block, int metadata, int modelID, RenderBlocks renderer) {

	}

	@Override
	public boolean renderWorldBlock(IBlockAccess world, int x, int y, int z, Block block, int modelId, RenderBlocks renderer) {
		return true;
	}

	@Override
	public boolean shouldRender3DInInventory() {
		return true;
	}

	@Override
	public int getRenderId() {
		return RenderIDs.renderIDLantern;
	}

	@Override
	public void renderTileEntityAt(TileEntity te, double x, double y, double z, float f) {
		if (te != null && te instanceof TileLantern) {
			TileLantern tile = (TileLantern) te;
			int color = tile.lanternmeta > 15 ? tile.lanternmeta - 16 : tile.lanternmeta;
			boolean isOn = tile.getLightValue() == 15;
			int rotation;
			// Bind the texture
			bindTextureForColorAndState(color, isOn);

			// Render the base that will always be rendered.
			renderLampAndCovers(x, y, z);

			// Render the stands that are orientation sensitive.
			renderStand(x, y, z, 0);
			
			// Render halo
			if (isOn) {
				renderLampShade(x, y, z, color);
			}
		}
	}

	public void bindTextureForColorAndState(int color, boolean on) {
		String base = "/mods/projectred/textures/blocks/";
		String folder = on ? "lanternon/" : "lanternoff/";
		String file = EnumLantern.get(color).unlocalName + (on ? "on" : "off");
		String loc = base + folder + file + ".png";
		bindTextureByName(loc);
	}

	public void renderLampAndCovers(double x, double y, double z) {
		// Render lantern bulb, and top and bottom covers.
		GL11.glPushMatrix();

		// Edit GL state
		// This will be rendered on basic state...

		GL11.glTranslated(x, y, z + 1);
		model.renderPart("lamp");
		model.renderPart("covertop");
		model.renderPart("coverbottom");

		GL11.glPopMatrix();

		// Undo GL state changes
		// Nothing to undo.
	}

	public void renderStand(double x, double y, double z, int rotation) {
		// Render lantern holder at the top.
		GL11.glPushMatrix();

		// Edit GL state
		// This will be rendered on basic state...

		GL11.glTranslated(x, y, z + 1);
		model.renderPart("goldring");
		model.renderPart("sidestand");

		GL11.glPopMatrix();

		// Undo GL state changes
		// Nothing to undo.

	}
	
	public void renderLampShade(double x, double y, double z, int tint) {
		// Render lamp shade
		GL11.glPushMatrix();

		// Edit GL state
		GL11.glDisable(GL11.GL_TEXTURE_2D);
		GL11.glEnable(GL11.GL_BLEND);
		GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE);
		GL11.glDisable(GL11.GL_LIGHTING);
		GL11.glDepthMask(false);

		GL11.glColor4f(Color.get(tint).r, Color.get(tint).g, Color.get(tint).b, 0.6f);
		GL11.glTranslated(x, y, z + 1);
		model.renderPart("lampshade");
		GL11.glPopMatrix();

		// Undo GL state changes
		GL11.glEnable(GL11.GL_TEXTURE_2D);
		GL11.glDisable(GL11.GL_BLEND);
		GL11.glBlendFunc(GL11.GL_ONE_MINUS_SRC_ALPHA, GL11.GL_ONE);
		GL11.glEnable(GL11.GL_LIGHTING);
		GL11.glDepthMask(true);
	}

}
