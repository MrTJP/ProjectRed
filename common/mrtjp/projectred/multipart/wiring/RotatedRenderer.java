package mrtjp.projectred.multipart.wiring;

import codechicken.core.render.CCRenderState;
import mrtjp.projectred.network.ClientProxy;
import mrtjp.projectred.renderstuffs.gates.RotatedPartModel;
import mrtjp.projectred.utils.Dir;
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

	/** START COMPLICATED MANUAL VERTEX ADDING **/
	private double[] quadBuffer = new double[20];
	private int bufferPos;
	private static int[][] rotationLookup = { { 9, 9, 0, 3, 2, 1 }, { 9, 9, 0, 3, 1, 2 }, { 3, 0, 9, 9, 2, 1 }, { 3, 0, 9, 9, 1, 2 }, { 3, 0, 1, 2, 9, 9 }, { 3, 0, 2, 1, 9, 9 }, };

	private void actuallyAddVertexWithUV(double x, double y, double z, double u, double v) {
		double temp;
		switch (side) {
		case Dir.NX:
		case Dir.PX:
			// x,y,z = y,z,x
			temp = y;
			y = z;
			z = x;
			x = temp;
			switch (rotationLookup[side][front]) {
			case 0:
				y = 1 - y;
				z = 1 - z;
				break;
			case 3:
				break;
			case 2:
				temp = y;
				y = z;
				z = 1 - temp;
				break;
			case 1:
				temp = y;
				y = 1 - z;
				z = temp;
				break;
			}
			if (side == Dir.PX) {
				x = 1 - x;
				z = 1 - z;
			}
			break;
		case Dir.NY:
		case Dir.PY:
			switch (rotationLookup[side][front]) {
			case 3:
				z = 1 - z;
				x = 1 - x;
				break;
			case 0:
				break;
			case 1:
				temp = z;
				z = x;
				x = 1 - temp;
				break;
			case 2:
				temp = z;
				z = 1 - x;
				x = temp;
				break;
			}
			if (side == Dir.PY) {
				y = 1 - y;
				x = 1 - x;
			}
			break;
		case Dir.NZ:
		case Dir.PZ:
			// x,y,z = x,z,y
			temp = z;
			z = y;
			y = temp;
			switch (rotationLookup[side][front]) {
			case 0:
				y = 1 - y;
				x = 1 - x;
				break;
			case 3:
				break;
			case 2:
				temp = y;
				y = x;
				x = 1 - temp;
				break;
			case 1:
				temp = y;
				y = 1 - x;
				x = temp;
				break;
			}
			if (side == Dir.PZ) {
				z = 1 - z;
				// y = 1-y;
			} else
				x = 1 - x;
			break;
		}
		base.addVertexWithUV(x + this.x, y + this.y, z + this.z, u, v);
	}

	public void addVertexWithUV(double x, double y, double z, double u, double v) {
		if (flipped) {
			x = 1 - x;
		}

		quadBuffer[bufferPos++] = x;
		quadBuffer[bufferPos++] = y;
		quadBuffer[bufferPos++] = z;
		quadBuffer[bufferPos++] = u;
		quadBuffer[bufferPos++] = v;

		if (bufferPos == 20) {
			bufferPos = 0;

			if (flipped) {
				// swap vertex order
				actuallyAddVertexWithUV(quadBuffer[15], quadBuffer[16], quadBuffer[17], quadBuffer[18], quadBuffer[19]);
				actuallyAddVertexWithUV(quadBuffer[10], quadBuffer[11], quadBuffer[12], quadBuffer[13], quadBuffer[14]);
				actuallyAddVertexWithUV(quadBuffer[5], quadBuffer[6], quadBuffer[7], quadBuffer[8], quadBuffer[9]);
				actuallyAddVertexWithUV(quadBuffer[0], quadBuffer[1], quadBuffer[2], quadBuffer[3], quadBuffer[4]);
			} else {
				// don't swap vertex order
				actuallyAddVertexWithUV(quadBuffer[0], quadBuffer[1], quadBuffer[2], quadBuffer[3], quadBuffer[4]);
				actuallyAddVertexWithUV(quadBuffer[5], quadBuffer[6], quadBuffer[7], quadBuffer[8], quadBuffer[9]);
				actuallyAddVertexWithUV(quadBuffer[10], quadBuffer[11], quadBuffer[12], quadBuffer[13], quadBuffer[14]);
				actuallyAddVertexWithUV(quadBuffer[15], quadBuffer[16], quadBuffer[17], quadBuffer[18], quadBuffer[19]);
			}
		}
	}
	/** STOP COMPLICATED MANUAL VERTEX ADDING **/

}
