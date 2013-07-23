package mrtjp.projectred.multipart.wiring;

import mrtjp.projectred.network.ClientProxy;
import mrtjp.projectred.renderstuffs.gates.RotatedPartModel;
import mrtjp.projectred.utils.codechicken.core.render.IUVTransformation;
import mrtjp.projectred.utils.codechicken.core.render.IconTransformation;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.util.Icon;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public class RotatedRenderer {
	public Tessellator base;
	public int front, side;
	public double x, y, z;
	public boolean flipped;
	public RenderBlocks renderBlocks;

	public void renderPartModel(RotatedPartModel model, String part, float xOffset, float yOffset, float zOffset, float degrotation, int color, boolean forceOriginalIcon) {
		IUVTransformation uv = null;
		Icon override = renderBlocks == null ? null : renderBlocks.overrideBlockTexture;
		
		if (forceOriginalIcon && ClientProxy.renderPass == 1 && override != null) {
			return;
		}
		if (override != null && !forceOriginalIcon) {
			uv = new IconTransformation(override);
		} else {
			uv = new IconTransformation(model.getIcon());
		}
		// Reset colors
		Tessellator.instance.setColorRGBA(255, 255, 255, 255);
		model.renderPart(part, (float) x, (float) y, (float) z, side, front, xOffset, yOffset, zOffset, degrotation, color, uv);
	}

	public void renderPartModelWithAlpha(RotatedPartModel model, String part, float xOffset, float yOffset, float zOffset, int color, int alpha) {
		IUVTransformation uv = null;
		Icon override = renderBlocks == null ? null : renderBlocks.overrideBlockTexture;
		if (override != null) {
			uv = new IconTransformation(override);
		} else {
			uv = new IconTransformation(model.getIcon());
		}
		model.renderPart(part, (float) x, (float) y, (float) z, side, front, xOffset, yOffset, zOffset, -1, -1, uv);
		// Reset colors
		Tessellator.instance.setColorRGBA(255, 255, 255, 255);
	}
}
