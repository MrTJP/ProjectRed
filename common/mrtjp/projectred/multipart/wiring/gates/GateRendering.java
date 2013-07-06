package mrtjp.projectred.multipart.wiring.gates;

import mrtjp.projectred.multipart.wiring.RotatedTessellator;
import mrtjp.projectred.multipart.wiring.wires.EnumWire;
import mrtjp.projectred.multipart.wiring.wires.WireRenderer;
import net.minecraft.block.Block;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.util.Icon;
import net.minecraft.util.Vec3;
import net.minecraft.util.Vec3Pool;
import net.minecraftforge.common.ForgeDirection;

public abstract class GateRendering {
	// Wire colours
	public static final int OFF = 0x400000;
	public static final int ON = 0xFF0000;
	public static final int DISABLED = 0xC0C0C0;

	public Icon torchTexOn = Block.torchRedstoneActive.getBlockTextureFromSide(0);
	public Icon torchTexOff = Block.torchRedstoneIdle.getBlockTextureFromSide(0);

	public String[] segmentTex = new String[] { "base" };
	public int[] segmentCol = new int[] { 0xFF0000 };

	// Torch positions are relative to the texture, in pixels
	public float[] torchX = new float[] {};
	public float[] torchY = new float[] {};
	public boolean[] torchState = new boolean[] {};

	// Ditto for pointer positions
	public float[] pointerX = new float[] {};
	public float[] pointerY = new float[] {};

	public void set(int renderState) {
	}

	public void setItemRender() {
	}

	public void customRender(RotatedTessellator rt, RenderBlocks render) {
	}

	public Icon[] segmentIcons = null;

	public void loadTextures(IconRegister register) {
		segmentIcons = new Icon[segmentTex.length];
		for (int k = 0; k < segmentTex.length; k++)
			segmentIcons[k] = register.registerIcon(ICON_PREFIX + segmentTex[k]);
	}

	public static final String ICON_PREFIX = "projectred:";

	/**
	 * Marker interface for renderings that don't store any state. Similar to
	 * GateLogic.Stateless. Currently not used.
	 */
	public static interface Stateless {
	}

	public static class Default extends GateRendering implements Stateless {

	}

	public static class AND extends GateRendering implements Stateless {
		{
			segmentTex = new String[] { "and-base", "and-ovl-out", "and-ovl-back", "and-ovl-right", "and-ovl-left" };
			segmentCol = new int[] { 0xFFFFFF, 0, 0, 0, 0 };
			torchX = new float[] { 8.5f, 4.5f, 8.5f, 12.5f };
			torchY = new float[] { 2.5f, 6.5f, 6.5f, 6.5f };
			torchState = new boolean[] { false, true, true, true };
		}

		@Override
		public void set(int renderState) {
			boolean out_on = (renderState & 1) != 0;
			boolean back_on = (renderState & 2) != 0;
			boolean left_on = (renderState & 4) != 0;
			boolean right_on = (renderState & 8) != 0;
			boolean left_disabled = (renderState & 16) != 0;
			boolean back_disabled = (renderState & 32) != 0;
			boolean right_disabled = (renderState & 64) != 0;
			segmentCol[1] = out_on ? OFF : ON;
			segmentCol[2] = back_disabled ? DISABLED : back_on ? ON : OFF;
			segmentCol[3] = right_disabled ? DISABLED : right_on ? ON : OFF;
			segmentCol[4] = left_disabled ? DISABLED : left_on ? ON : OFF;
			torchState[0] = out_on;
			torchState[1] = !left_on && !left_disabled;
			torchState[2] = !back_on && !back_disabled;
			torchState[3] = !right_on && !right_disabled;
		}

		@Override
		public void setItemRender() {
			segmentCol[1] = ON;
			segmentCol[2] = OFF;
			segmentCol[3] = OFF;
			segmentCol[4] = OFF;
			torchState[0] = false;
			torchState[1] = true;
			torchState[2] = true;
			torchState[3] = true;
		}
	}

	public static class OR extends GateRendering implements Stateless {
		{
			segmentTex = new String[] { "or-base", "or-ovl-out", "or-ovl-back", "or-ovl-right", "or-ovl-left", "or-ovl-middle" };
			segmentCol = new int[] { 0xFFFFFF, 0, 0, 0, 0, 0xFF0000 };
			torchX = new float[] { 7.5f, 7.5f };
			torchY = new float[] { 5.5f, 9.5f };
			torchState = new boolean[] { false, true };
		}

		@Override
		public void set(int renderState) {
			segmentCol[1] = (renderState & 1) != 0 ? ON : OFF;
			segmentCol[2] = (renderState & 64) != 0 ? DISABLED : (renderState & 2) != 0 ? ON : OFF;
			segmentCol[3] = (renderState & 128) != 0 ? DISABLED : (renderState & 8) != 0 ? ON : OFF;
			segmentCol[4] = (renderState & 32) != 0 ? DISABLED : (renderState & 4) != 0 ? ON : OFF;
			segmentCol[5] = (renderState & 1) == 0 ? ON : OFF;
			torchState[0] = (renderState & 16) != 0;
			torchState[1] = !torchState[0];
		}

		@Override
		public void setItemRender() {
			segmentCol[1] = OFF;
			segmentCol[2] = OFF;
			segmentCol[3] = OFF;
			segmentCol[4] = OFF;
			segmentCol[5] = ON;
			torchState[0] = false;
			torchState[1] = true;
		}
	}

	public static class NOT extends GateRendering implements Stateless {
		{
			segmentTex = new String[] { "not-base", "not-ovl-out", "not-ovl-back", "not-ovl-right", "not-ovl-left" };
			segmentCol = new int[] { 0xFFFFFF, 0, 0, 0, 0 };
			torchX = new float[] { 7.5f };
			torchY = new float[] { 7.5f };
			torchState = new boolean[] { false };
		}

		@Override
		public void set(int renderState) {
			segmentCol[1] = (renderState & 64) != 0 ? DISABLED : (renderState & 1) != 0 ? ON : OFF;
			segmentCol[2] = (renderState & 2) != 0 ? ON : OFF;
			segmentCol[3] = (renderState & 128) != 0 ? DISABLED : (renderState & 8) != 0 ? ON : OFF;
			segmentCol[4] = (renderState & 32) != 0 ? DISABLED : (renderState & 4) != 0 ? ON : OFF;
			torchState[0] = (renderState & 16) != 0;
		}

		@Override
		public void setItemRender() {
			segmentCol[1] = segmentCol[3] = segmentCol[4] = ON;
			segmentCol[2] = OFF;
			torchState[0] = true;
		}
	}

	public static class RSLatch extends GateRendering implements Stateless {
		{
			segmentTex = new String[] { "rs-base", "rs-left", "rs-right" };
			segmentCol = new int[] { 0xFFFFFF, 0, 0 };
			torchX = new float[] { 6.5f, 9.5f };
			torchY = new float[] { 3.5f, 12.5f };
			torchState = new boolean[] { false, false };
		}

		@Override
		public void setItemRender() {
			segmentCol[1] = ON;
			segmentCol[2] = OFF;
			torchState[0] = true;
			torchState[1] = false;
		}

		@Override
		public void set(int renderState) {
			segmentCol[1] = (renderState & 1) != 0 ? ON : OFF;
			segmentCol[2] = (renderState & 2) != 0 ? ON : OFF;
			torchState[0] = (renderState & 4) != 0;
			torchState[1] = (renderState & 8) != 0;
		}
	}

	public static class ToggleLatch extends GateRendering implements Stateless {
		{
			segmentTex = new String[] { "toggle-base", "toggle-left", "toggle-right" };
			segmentCol = new int[] { 0xFFFFFF, 0, 0 };
			torchX = new float[] { 4.5f, 4.5f };
			torchY = new float[] { 3.5f, 12.5f };
			torchState = new boolean[] { true, false };
		}

		@Override
		public void setItemRender() {
			segmentCol[1] = OFF;
			segmentCol[2] = OFF;
			torchState[0] = true;
			torchState[1] = false;
		}

		@Override
		public void set(int renderState) {
			segmentCol[1] = (renderState & 1) != 0 ? ON : OFF;
			segmentCol[2] = (renderState & 2) != 0 ? ON : OFF;
			torchState[0] = (renderState & 4) != 0;
			torchState[1] = (renderState & 8) != 0;
		}

		@Override
		public void customRender(RotatedTessellator rt, RenderBlocks render) {
			boolean leverDirection = !torchState[1];

			// size: 6x8 pixels
			double minX = 7 / 16., minZ = 4 / 16., maxX = 13 / 16., maxZ = 12 / 16., minY = BlockGate.THICKNESS, maxY = 4 / 16.;

			rt.base.setColorOpaque(255, 255, 255);

			{
				// draw lever base

				/*
				 * double minX = 8/16.; double maxX = 11/16.; double minZ =
				 * 5/16.; double maxZ = 11/16.; double minY =
				 * GateBlock.THICKNESS; double maxY = minY + 2/16.;
				 */

				Icon tex = render.hasOverrideBlockTexture() ? render.overrideBlockTexture : Block.cobblestone.getIcon(0, 0);

				rt.addVertexWithUV(minX, minY, minZ, tex.getInterpolatedU(minX * 16), tex.getInterpolatedV(minY * 16));
				rt.addVertexWithUV(minX, maxY, minZ, tex.getInterpolatedU(minX * 16), tex.getInterpolatedV(maxY * 16));
				rt.addVertexWithUV(maxX, maxY, minZ, tex.getInterpolatedU(maxX * 16), tex.getInterpolatedV(maxY * 16));
				rt.addVertexWithUV(maxX, minY, minZ, tex.getInterpolatedU(maxX * 16), tex.getInterpolatedV(minY * 16));

				rt.addVertexWithUV(maxX, minY, maxZ, tex.getInterpolatedU(maxX * 16), tex.getInterpolatedV(minY * 16));
				rt.addVertexWithUV(maxX, maxY, maxZ, tex.getInterpolatedU(maxX * 16), tex.getInterpolatedV(maxY * 16));
				rt.addVertexWithUV(minX, maxY, maxZ, tex.getInterpolatedU(minX * 16), tex.getInterpolatedV(maxY * 16));
				rt.addVertexWithUV(minX, minY, maxZ, tex.getInterpolatedU(minX * 16), tex.getInterpolatedV(minY * 16));

				rt.addVertexWithUV(maxX, minY, minZ, tex.getInterpolatedU(minZ * 16), tex.getInterpolatedV(minY * 16));
				rt.addVertexWithUV(maxX, maxY, minZ, tex.getInterpolatedU(minZ * 16), tex.getInterpolatedV(maxY * 16));
				rt.addVertexWithUV(maxX, maxY, maxZ, tex.getInterpolatedU(maxZ * 16), tex.getInterpolatedV(maxY * 16));
				rt.addVertexWithUV(maxX, minY, maxZ, tex.getInterpolatedU(maxZ * 16), tex.getInterpolatedV(minY * 16));

				rt.addVertexWithUV(minX, minY, maxZ, tex.getInterpolatedU(maxZ * 16), tex.getInterpolatedV(minY * 16));
				rt.addVertexWithUV(minX, maxY, maxZ, tex.getInterpolatedU(maxZ * 16), tex.getInterpolatedV(maxY * 16));
				rt.addVertexWithUV(minX, maxY, minZ, tex.getInterpolatedU(minZ * 16), tex.getInterpolatedV(maxY * 16));
				rt.addVertexWithUV(minX, minY, minZ, tex.getInterpolatedU(minZ * 16), tex.getInterpolatedV(minY * 16));

				rt.addVertexWithUV(minX, maxY, minZ, tex.getInterpolatedU(minX * 16), tex.getInterpolatedV(minZ * 16));
				rt.addVertexWithUV(minX, maxY, maxZ, tex.getInterpolatedU(minX * 16), tex.getInterpolatedV(maxZ * 16));
				rt.addVertexWithUV(maxX, maxY, maxZ, tex.getInterpolatedU(maxX * 16), tex.getInterpolatedV(maxZ * 16));
				rt.addVertexWithUV(maxX, maxY, minZ, tex.getInterpolatedU(maxX * 16), tex.getInterpolatedV(minZ * 16));
			}

			// render lever handle
			{
				Icon tex = Block.lever.getIcon(0, 0);

				double d0 = (double) tex.getMinU();
				double d1 = (double) tex.getMinV();
				double d2 = (double) tex.getMaxU();
				double d3 = (double) tex.getMaxV();
				Vec3[] avec3 = new Vec3[8];
				float f4 = 0.0625F;
				float f5 = 0.0625F;
				float f6 = 0.625F;
				Vec3Pool pool;
				if (render.blockAccess == null)
					pool = Vec3.fakePool;
				else
					pool = render.blockAccess.getWorldVec3Pool();
				avec3[0] = pool.getVecFromPool((double) (-f4), 0.0D, (double) (-f5));
				avec3[1] = pool.getVecFromPool((double) f4, 0.0D, (double) (-f5));
				avec3[2] = pool.getVecFromPool((double) f4, 0.0D, (double) f5);
				avec3[3] = pool.getVecFromPool((double) (-f4), 0.0D, (double) f5);
				avec3[4] = pool.getVecFromPool((double) (-f4), (double) f6, (double) (-f5));
				avec3[5] = pool.getVecFromPool((double) f4, (double) f6, (double) (-f5));
				avec3[6] = pool.getVecFromPool((double) f4, (double) f6, (double) f5);
				avec3[7] = pool.getVecFromPool((double) (-f4), (double) f6, (double) f5);

				int i1 = 5; // or 6

				for (int j1 = 0; j1 < 8; ++j1) {
					if (leverDirection) {
						// avec3[j1].zCoord -= 0.0625D;
						avec3[j1].rotateAroundX(((float) Math.PI * 2F / 9F));
					} else {
						// avec3[j1].zCoord += 0.0625D;
						avec3[j1].rotateAroundX(-((float) Math.PI * 2F / 9F));
					}

					if (i1 == 6) {
						avec3[j1].rotateAroundY(((float) Math.PI / 2F));
					}

					else if (i1 != 0 && i1 != 7) {
						/*
						 * avec3[j1].xCoord += (double)par2 + 0.5D;
						 * avec3[j1].yCoord += (double)((float)par3 + 0.125F);
						 * avec3[j1].zCoord += (double)par4 + 0.5D;
						 */
						avec3[j1].xCoord += (minX + maxX) / 2;
						avec3[j1].yCoord += 0.125f;
						avec3[j1].zCoord += (minZ + maxZ) / 2;
					}
				}

				Vec3 vec3 = null;
				Vec3 vec31 = null;
				Vec3 vec32 = null;
				Vec3 vec33 = null;

				for (int k1 = 0; k1 < 6; ++k1) {
					if (k1 == 0) {
						d0 = (double) tex.getInterpolatedU(7.0D);
						d1 = (double) tex.getInterpolatedV(6.0D);
						d2 = (double) tex.getInterpolatedU(9.0D);
						d3 = (double) tex.getInterpolatedV(8.0D);
					} else if (k1 == 2) {
						d0 = (double) tex.getInterpolatedU(7.0D);
						d1 = (double) tex.getInterpolatedV(6.0D);
						d2 = (double) tex.getInterpolatedU(9.0D);
						d3 = (double) tex.getMaxV();
					}

					if (k1 == 0) {
						vec3 = avec3[0];
						vec31 = avec3[1];
						vec32 = avec3[2];
						vec33 = avec3[3];
					} else if (k1 == 1) {
						vec3 = avec3[7];
						vec31 = avec3[6];
						vec32 = avec3[5];
						vec33 = avec3[4];
					} else if (k1 == 2) {
						vec3 = avec3[1];
						vec31 = avec3[0];
						vec32 = avec3[4];
						vec33 = avec3[5];
					} else if (k1 == 3) {
						vec3 = avec3[2];
						vec31 = avec3[1];
						vec32 = avec3[5];
						vec33 = avec3[6];
					} else if (k1 == 4) {
						vec3 = avec3[3];
						vec31 = avec3[2];
						vec32 = avec3[6];
						vec33 = avec3[7];
					} else if (k1 == 5) {
						vec3 = avec3[0];
						vec31 = avec3[3];
						vec32 = avec3[7];
						vec33 = avec3[4];
					}

					rt.addVertexWithUV(vec3.xCoord, vec3.yCoord, vec3.zCoord, d0, d3);
					rt.addVertexWithUV(vec31.xCoord, vec31.yCoord, vec31.zCoord, d2, d3);
					rt.addVertexWithUV(vec32.xCoord, vec32.yCoord, vec32.zCoord, d2, d1);
					rt.addVertexWithUV(vec33.xCoord, vec33.yCoord, vec33.zCoord, d0, d1);
				}
			}
		}
	}

	public static class NOR extends GateRendering implements Stateless {
		{
			segmentTex = new String[] { "nor-base", "nor-out", "nor-back", "nor-right", "nor-left" };
			segmentCol = new int[] { 0xFFFFFF, 0, 0, 0, 0 };
			torchX = new float[] { 7.5f };
			torchY = new float[] { 7.5f };
			torchState = new boolean[] { false };
		}

		@Override
		public void set(int renderState) {
			segmentCol[1] = (renderState & 1) != 0 ? ON : OFF;
			segmentCol[2] = (renderState & 64) != 0 ? DISABLED : (renderState & 2) != 0 ? ON : OFF;
			segmentCol[3] = (renderState & 128) != 0 ? DISABLED : (renderState & 8) != 0 ? ON : OFF;
			segmentCol[4] = (renderState & 32) != 0 ? DISABLED : (renderState & 4) != 0 ? ON : OFF;
			torchState[0] = (renderState & 16) != 0;
		}

		@Override
		public void setItemRender() {
			segmentCol[1] = segmentCol[3] = segmentCol[4] = ON;
			segmentCol[2] = OFF;
			torchState[0] = true;
		}
	}

	public static class NAND extends GateRendering implements Stateless {
		{
			segmentTex = new String[] { "nand-base", "nand-out", "nand-back", "nand-right", "nand-left" };
			segmentCol = new int[] { 0xFFFFFF, 0, 0, 0, 0 };
			torchX = new float[] { 4.5f, 8.5f, 12.5f };
			torchY = new float[] { 6.5f, 6.5f, 6.5f };
			torchState = new boolean[] { true, true, true };
		}

		@Override
		public void set(int renderState) {
			boolean left_disabled = (renderState & 16) != 0;
			boolean back_disabled = (renderState & 32) != 0;
			boolean right_disabled = (renderState & 64) != 0;
			segmentCol[1] = (renderState & 1) != 0 ? ON : OFF;
			segmentCol[2] = back_disabled ? DISABLED : (renderState & 2) != 0 ? ON : OFF;
			segmentCol[3] = right_disabled ? DISABLED : (renderState & 8) != 0 ? ON : OFF;
			segmentCol[4] = left_disabled ? DISABLED : (renderState & 4) != 0 ? ON : OFF;
			torchState[0] = (renderState & 4) == 0 && !left_disabled;
			torchState[1] = (renderState & 2) == 0 && !back_disabled;
			torchState[2] = (renderState & 8) == 0 && !right_disabled;
		}

		@Override
		public void setItemRender() {
			segmentCol[1] = ON;
			segmentCol[2] = OFF;
			segmentCol[3] = OFF;
			segmentCol[4] = OFF;
			torchState[0] = true;
			torchState[1] = true;
			torchState[2] = true;
		}
	}

	public static class XOR extends GateRendering implements Stateless {
		{
			segmentTex = new String[] { "xor-base", "xor-left", "xor-right", "xor-middle", "xor-out" };
			segmentCol = new int[] { 0xFFFFFF, 0, 0, 0, 0 };
			torchX = new float[] { 4.5f, 11.5f, 8.5f };
			torchY = new float[] { 9.5f, 9.5f, 13.5f };
			torchState = new boolean[] { false, false, false };
		}

		@Override
		public void set(int renderState) {
			boolean left = (renderState & 1) != 0;
			boolean right = (renderState & 2) != 0;
			boolean out = (renderState & 4) != 0;
			segmentCol[1] = left ? ON : OFF;
			segmentCol[2] = right ? ON : OFF;
			segmentCol[3] = !left && !right ? ON : OFF;
			segmentCol[4] = out ? ON : OFF;
			torchState[0] = !left && right;
			torchState[1] = left && !right;
			torchState[2] = !left && !right;
		}

		@Override
		public void setItemRender() {
			segmentCol[1] = OFF;
			segmentCol[2] = OFF;
			segmentCol[3] = ON;
			segmentCol[4] = OFF;
			torchState[0] = false;
			torchState[1] = false;
			torchState[2] = true;
		}
	}

	public static class XNOR extends GateRendering implements Stateless {
		{
			segmentTex = new String[] { "xnor-base", "xnor-left", "xnor-right", "xnor-middle", "xnor-left-out", "xnor-right-out" };
			segmentCol = new int[] { 0xFFFFFF, 0, 0, 0, 0, 0 };
			torchX = new float[] { 8.0f, 4.5f, 11.5f, 8.5f };
			torchY = new float[] { 3.0f, 9.5f, 9.5f, 13.5f };
			torchState = new boolean[] { false, false, false, false };
		}

		@Override
		public void set(int renderState) {
			boolean left = (renderState & 1) != 0;
			boolean right = (renderState & 2) != 0;
			boolean out = (renderState & 4) != 0;
			segmentCol[1] = left ? ON : OFF;
			segmentCol[2] = right ? ON : OFF;
			segmentCol[3] = !left && !right ? ON : OFF;
			segmentCol[4] = !left && right ? ON : OFF;
			segmentCol[5] = left && !right ? ON : OFF;
			torchState[0] = out;
			torchState[1] = !left && right;
			torchState[2] = left && !right;
			torchState[3] = !left && !right;
		}

		@Override
		public void setItemRender() {
			segmentCol[1] = OFF;
			segmentCol[2] = OFF;
			segmentCol[3] = ON;
			segmentCol[4] = OFF;
			segmentCol[5] = OFF;
			torchState[0] = true;
			torchState[1] = false;
			torchState[2] = false;
			torchState[3] = true;
		}
	}

	public static class Buffer extends GateRendering implements Stateless {
		{
			segmentTex = new String[] { "buffer-base", "buffer-left", "buffer-right", "buffer-back", "buffer-out" };
			segmentCol = new int[] { 0xFFFFFF, 0, 0, 0, 0 };
			torchX = new float[] { 7.5f, 7.5f };
			torchY = new float[] { 2.5f, 9.5f };
			torchState = new boolean[] { false, false };
		}

		@Override
		public void set(int renderState) {
			segmentCol[1] = (renderState & 4) != 0 ? ON : OFF;
			segmentCol[2] = (renderState & 8) != 0 ? ON : OFF;
			segmentCol[3] = (renderState & 2) != 0 ? ON : OFF;
			segmentCol[4] = (renderState & 2) == 0 ? ON : OFF;
			torchState[0] = (renderState & 1) != 0;
			torchState[1] = (renderState & 2) == 0;
		}

		@Override
		public void setItemRender() {
			segmentCol[1] = OFF;
			segmentCol[2] = OFF;
			segmentCol[3] = OFF;
			segmentCol[4] = ON;
			torchState[0] = false;
			torchState[1] = true;
		}
	}

	public static class Multiplexer extends GateRendering implements Stateless {
		{
			segmentTex = new String[] { "multiplexer-base", "multiplexer-2", "multiplexer-3", "multiplexer-right", "multiplexer-5", "multiplexer-left-out", "multiplexer-right-out" };
			segmentCol = new int[] { 0xFFFFFF, 0, 0, 0, 0, 0, 0 };
			torchX = new float[] { 8f, 4.5f, 11.5f, 4.5f };
			torchY = new float[] { 2f, 7.5f, 7.5f, 12.5f };
			torchState = new boolean[] { false, false, false, false };
		}

		@Override
		public void set(int renderState) {
			boolean back = (renderState & 1) != 0;
			boolean left = (renderState & 2) != 0;
			boolean right = (renderState & 4) != 0;
			boolean out = (renderState & 8) != 0;
			segmentCol[1] = back ? ON : OFF;
			segmentCol[2] = left ? ON : OFF;
			segmentCol[3] = right ? ON : OFF;
			segmentCol[4] = !back ? ON : OFF;
			segmentCol[5] = !left && back ? ON : OFF;
			segmentCol[6] = !right && !back ? ON : OFF;
			torchState[0] = out;
			torchState[1] = !left && back;
			torchState[2] = !right && !back;
			torchState[3] = !back;
		}

		@Override
		public void setItemRender() {
			segmentCol[1] = OFF;
			segmentCol[2] = OFF;
			segmentCol[3] = OFF;
			segmentCol[4] = ON;
			segmentCol[5] = OFF;
			segmentCol[6] = ON;
			torchState[0] = false;
			torchState[1] = false;
			torchState[2] = true;
			torchState[3] = true;
		}
	}

	public static class Repeater extends GateRendering implements Stateless {
		{
			segmentTex = new String[] { "repeater-base", "repeater-strip" };
			segmentCol = new int[] { 0xFFFFFF, 0 };
			torchX = new float[] { 8f, 8f };
			torchY = new float[] { 3f, 6f };
			torchState = new boolean[] { false, false };
		}

		@Override
		public void set(int renderState) {
			boolean out = (renderState & 32768) != 0;

			torchY[1] = (renderState & 7) + 6;

			torchState[0] = torchState[1] = out;

			segmentCol[1] = out ? ON : OFF;
		}

		@Override
		public void setItemRender() {
			torchY[1] = 6f;

			torchState[0] = torchState[1] = false;

			segmentCol[1] = OFF;
		}
	}

	public static class Timer extends GateRendering implements Stateless {
		{
			segmentTex = new String[] { "timer-base", "timer-left", "timer-back", "timer-right" };
			segmentCol = new int[] { 0xFFFFFF, 0, 0, 0 };
			torchX = new float[] { 8f };
			torchY = new float[] { 2f };
			torchState = new boolean[] { false };
			pointerX = new float[] { 8f };
			pointerY = new float[] { 8f };
		}

		@Override
		public void set(int renderState) {
			torchState[0] = (renderState & 1) != 0;
			segmentCol[1] = (renderState & 2) != 0 ? ON : OFF;
			segmentCol[2] = (renderState & 4) != 0 ? ON : OFF;
			segmentCol[3] = (renderState & 8) != 0 ? ON : OFF;
		}

		@Override
		public void setItemRender() {
			torchState[0] = false;
			segmentCol[1] = OFF;
			segmentCol[2] = OFF;
			segmentCol[3] = OFF;
		}
	}

	public static class Counter extends GateRendering implements Stateless {
		{
			segmentTex = new String[] { "counter-base", "counter-front", "counter-back" };
			segmentCol = new int[] { 0xFFFFFF, 0, 0 };
			torchX = new float[] { 3f, 13f };
			torchY = new float[] { 8f, 8f };
			torchState = new boolean[] { false, false };
			pointerX = new float[] { 8f };
			pointerY = new float[] { 11f };
		}

		@Override
		public void set(int renderState) {
			segmentCol[1] = (renderState & 1) != 0 ? ON : OFF;
			segmentCol[2] = (renderState & 2) != 0 ? ON : OFF;
			torchState[0] = (renderState & 4) != 0;
			torchState[1] = (renderState & 8) != 0;
		}

		@Override
		public void setItemRender() {
			segmentCol[1] = OFF;
			segmentCol[2] = OFF;
			torchState[0] = true;
			torchState[1] = false;
		}
	}

	public static class Sequencer extends GateRendering implements Stateless {
		{
			segmentTex = new String[] { "sequencer-base" };
			segmentCol = new int[] { 0xFFFFFF };
			torchX = new float[] { 8f, 2f, 14f, 8f };
			torchY = new float[] { 2f, 8f, 8f, 14f };
			torchState = new boolean[] { false, false, false, false };
			pointerX = new float[] { 8f };
			pointerY = new float[] { 8f };
		}

		@Override
		public void set(int renderState) {
			torchState[0] = renderState == 0;
			torchState[1] = renderState == 3;
			torchState[2] = renderState == 1;
			torchState[3] = renderState == 2;
		}

		@Override
		public void setItemRender() {
			torchState[0] = true;
			torchState[1] = false;
			torchState[2] = false;
			torchState[3] = false;
		}
	}

	public static class PulseFormer extends GateRendering implements Stateless {
		{
			segmentTex = new String[] { "former-base", "former-in", "former-3", "former-4", "former-5" };
			segmentCol = new int[] { 0xFFFFFF, 0, 0, 0, 0 };
			torchX = new float[] { 8f, 4.5f, 11.5f };
			torchY = new float[] { 2f, 8f, 8f };
			torchState = new boolean[] { false, false, false };
		}

		@Override
		public void set(int renderState) {
			boolean in = (renderState & 1) != 0;
			boolean out = (renderState & 2) != 0;
			boolean changing = (renderState & 4) != 0;
			torchState[0] = out;
			torchState[1] = !in;
			torchState[2] = in && (!out || changing);
			segmentCol[1] = in ? ON : OFF;
			segmentCol[2] = in ? OFF : ON;
			segmentCol[3] = in ? OFF : ON;
			segmentCol[4] = /* in && !out */torchState[2] ? ON : OFF;
		}

		@Override
		public void setItemRender() {
			torchState[0] = false;
			torchState[1] = true;
			torchState[2] = false;
			segmentCol[1] = OFF;
			segmentCol[2] = ON;
			segmentCol[3] = ON;
			segmentCol[4] = OFF;
		}
	}

	public static class Randomizer extends GateRendering {
		{
			torchX = new float[] { 8f, 3f, 13f };
			torchY = new float[] { 3f, 8f, 8f };
			torchState = new boolean[] { false, false, false };
			segmentTex = new String[] { "randomizer-base", "randomizer-in" };
			segmentCol = new int[] { 0xFFFFFF, 0 };
		}

		@Override
		public void loadTextures(IconRegister register) {
			super.loadTextures(register);

			torchTexOn = register.registerIcon(ICON_PREFIX + "randomizer-torch-on");
			torchTexOff = register.registerIcon(ICON_PREFIX + "randomizer-torch-off");
		}

		@Override
		public void set(int renderState) {
			segmentCol[1] = (renderState & 1) != 0 ? ON : OFF;
			torchState[0] = (renderState & 8) != 0;
			torchState[1] = (renderState & 2) != 0;
			torchState[2] = (renderState & 4) != 0;
		}

		@Override
		public void setItemRender() {
			segmentCol[1] = OFF;
			torchState[0] = false;
			torchState[1] = false;
			torchState[2] = false;
		}
	}

	public static class StateCell extends GateRendering implements Stateless {
		{
			segmentTex = new String[] { "statecell-base", "statecell-2", "statecell-3", "statecell-4", "statecell-5", "statecell-6", "statecell-7" };
			segmentCol = new int[] { 0xFFFFFF, 0, 0, 0, 0, 0, 0 };
			torchX = new float[] { 13f };
			torchY = new float[] { 7f };
			torchState = new boolean[] { false };
			pointerX = new float[] { 8f };
			pointerY = new float[] { 12f };
		}

		@Override
		public void set(int renderState) {
			segmentCol[1] = (renderState & 1) != 0 ? ON : OFF;
			segmentCol[2] = (renderState & 2) != 0 ? ON : OFF;
			segmentCol[3] = (renderState & 4) != 0 ? ON : OFF;
			segmentCol[4] = (renderState & 16) == 0 ? ON : OFF;
			segmentCol[5] = (renderState & 16) != 0 ? ON : OFF;
			segmentCol[6] = (renderState & 8) != 0 ? ON : OFF;
			torchState[0] = (renderState & 8) != 0;
		}

		@Override
		public void setItemRender() {
			segmentCol[1] = OFF;
			segmentCol[2] = OFF;
			segmentCol[3] = OFF;
			segmentCol[4] = ON;
			segmentCol[5] = OFF;
			segmentCol[6] = OFF;
			torchState[0] = false;
		}
	}

	public static class Synchronizer extends GateRendering implements Stateless {
		{
			segmentCol = new int[] { 0xFFFFFF, 0, 0, 0, 0, 0, 0 };
			segmentTex = new String[] { "sync-base", "sync-left", "sync-right", "sync-back", "sync-middle", "sync-left-middle", "sync-right-middle" };
			torchX = new float[] { 8f };
			torchY = new float[] { 3f };
			torchState = new boolean[] { false };
		}

		@Override
		public void set(int renderState) {
			segmentCol[1] = (renderState & 1) != 0 ? ON : OFF;
			segmentCol[2] = (renderState & 2) != 0 ? ON : OFF;
			segmentCol[3] = (renderState & 4) != 0 ? ON : OFF;
			torchState[0] = (renderState & 8) != 0;
			segmentCol[4] = (renderState & 48) != 0 ? ON : OFF;
			segmentCol[5] = (renderState & 16) != 0 ? OFF : ON;
			segmentCol[6] = (renderState & 32) != 0 ? OFF : ON;
		}

		@Override
		public void setItemRender() {
			segmentCol[1] = OFF;
			segmentCol[2] = OFF;
			segmentCol[3] = OFF;
			segmentCol[4] = OFF;
			segmentCol[5] = ON;
			segmentCol[6] = ON;
			torchState[0] = false;
		}
	}

	public static class DLatch extends GateRendering implements Stateless {
		{
			segmentCol = new int[] { 0xFFFFFF, 0, 0, 0 };
			segmentTex = new String[] { "dlatch-base", "dlatch-in", "dlatch-enable", "dlatch-out" };
		}

		@Override
		public void set(int renderState) {
			segmentCol[1] = (renderState & 1) != 0 ? ON : OFF;
			segmentCol[2] = (renderState & 2) != 0 ? ON : OFF;
			segmentCol[3] = (renderState & 4) != 0 ? ON : OFF;
		}

		@Override
		public void setItemRender() {
			segmentCol[1] = OFF;
			segmentCol[2] = OFF;
			segmentCol[3] = OFF;
		}
	}

	public static class DFlop extends GateRendering implements Stateless {
		{
			segmentCol = new int[] { 0xFFFFFF, 0, 0, 0 };
			segmentTex = new String[] { "dflop-base", "dflop-in", "dflop-enable", "dflop-out" };
		}

		@Override
		public void set(int renderState) {
			segmentCol[1] = (renderState & 1) != 0 ? ON : OFF;
			segmentCol[2] = (renderState & 2) != 0 ? ON : OFF;
			segmentCol[3] = (renderState & 4) != 0 ? ON : OFF;
		}

		@Override
		public void setItemRender() {
			segmentCol[1] = OFF;
			segmentCol[2] = OFF;
			segmentCol[3] = OFF;
		}
	}
	
	public static class BundledLatch extends GateRendering implements Stateless {
		{
			segmentCol = new int[] {0xFFFFFF, 0};
			segmentTex = new String[] {"blatch-base", "blatch-enable"};
		}
		
		@Override
		public void set(int renderState) {
			segmentCol[1] = (renderState & 1) != 0 ? ON : OFF;
		}
		
		@Override
		public void setItemRender() {
			segmentCol[1] = OFF;
		}
		
		@Override
		public void customRender(RotatedTessellator rt, RenderBlocks render) {
			ForgeDirection up = ForgeDirection.VALID_DIRECTIONS[rt.side ^ 1];
			
			// draw a bundled cable, offset away from the base of the gate
			double offset = 0; // was 0.125
			double dx = up.offsetX * offset, dy = up.offsetY * offset, dz = up.offsetZ * offset;
			rt.x += dx; rt.y += dy; rt.z += dz;
			WireRenderer.renderWireSide(rt, render, EnumWire.BUNDLED, true, true, false, false, false, false, false, false, null, null, null, null, true, false);
			rt.x -= dx; rt.y -= dy; rt.z -= dz;
			
			renderRaisedSquare(rt, render, segmentIcons[0], 4, 4, 12, 12, 3 + (int)(offset * 16 + 0.5));
		}
	}
	
	public static class BundledMultiplexer extends GateRendering implements Stateless {
		{
			segmentCol = new int[] {0xFFFFFF, 0};
			segmentTex = new String[] {"bmulti-left", "bmulti-select"};
		}
		
		private Icon iconLeft, iconRight;
		
		@Override
		public void loadTextures(IconRegister register) {
			super.loadTextures(register);
			iconLeft = segmentIcons[0];
			iconRight = register.registerIcon(ICON_PREFIX + "bmulti-right");
		}
		
		@Override
		public void set(int renderState) {
			segmentCol[1] = (renderState & 1) != 0 ? ON : OFF;
			segmentIcons[0] = (renderState & 1) != 0 ? iconRight : iconLeft;
		}
		
		@Override
		public void setItemRender() {
			segmentCol[1] = OFF;
			segmentIcons[0] = iconLeft;
		}
		
		@Override
		public void customRender(RotatedTessellator rt, RenderBlocks render) {
			ForgeDirection up = ForgeDirection.VALID_DIRECTIONS[rt.side ^ 1];
			
			// draw a bundled cable, offset away from the base of the gate
			double offset = 0; // was 0.125
			double dx = up.offsetX * offset, dy = up.offsetY * offset, dz = up.offsetZ * offset;
			rt.x += dx; rt.y += dy; rt.z += dz;
			WireRenderer.renderWireSide(rt, render, EnumWire.BUNDLED, true, false, true, true, false, false, false, false, null, null, null, null, true, false);
			rt.x -= dx; rt.y -= dy; rt.z -= dz;
			
			renderRaisedSquare(rt, render, segmentIcons[0], 4, 4, 12, 12, 3 + (int)(offset * 16 + 0.5));
		}
	}
	
	public static class BundledRelay extends GateRendering implements Stateless {
		{
			segmentCol = new int[] {0xFFFFFF, 0};
			segmentTex = new String[] {"brelay-off", "brelay-enable"};
		}
		
		private Icon icon_on, icon_off;
		
		@Override
		public void loadTextures(IconRegister register) {
			super.loadTextures(register);
			icon_on = register.registerIcon(ICON_PREFIX + "brelay-on");
			icon_off = segmentIcons[0];
		}
		
		@Override
		public void set(int renderState) {
			segmentCol[1] = (renderState & 1) != 0 ? ON : OFF;
			segmentIcons[0] = (renderState & 1) != 0 ? icon_on : icon_off;
		}
		
		@Override
		public void setItemRender() {
			segmentCol[1] = OFF;
			segmentIcons[0] = icon_off;
		}
		
		@Override
		public void customRender(RotatedTessellator rt, RenderBlocks render) {
			ForgeDirection up = ForgeDirection.VALID_DIRECTIONS[rt.side ^ 1];
			
			// draw a bundled cable, offset away from the base of the gate
			double offset = 0; // was 0.125
			double dx = up.offsetX * offset, dy = up.offsetY * offset, dz = up.offsetZ * offset;
			rt.x += dx; rt.y += dy; rt.z += dz;
			WireRenderer.renderWireSide(rt, render, EnumWire.BUNDLED, true, true, false, false, false, false, false, false, null, null, null, null, true, false);
			rt.x -= dx; rt.y -= dy; rt.z -= dz;
			
			renderRaisedSquare(rt, render, segmentIcons[0], 4, 4, 12, 12, 3 + (int)(offset * 16 + 0.5));
		}
	}

	protected void renderRaisedSquare(RotatedTessellator rt, RenderBlocks render, Icon tex, int left, int top, int right, int bottom, int thickness) {
		double minX = left/16., maxX = right/16., minZ = top/16., maxZ = bottom/16.;
		double minY = 1.0/8.0, maxY = minY + thickness/16.0;
		
		double minU = tex.getInterpolatedU(left), maxU = tex.getInterpolatedU(right);
		double minV = tex.getInterpolatedV(top), maxV = tex.getInterpolatedV(bottom);
		
		rt.addVertexWithUV(minX, maxY, maxZ, minU, maxV);
		rt.addVertexWithUV(maxX, maxY, maxZ, maxU, maxV);
		rt.addVertexWithUV(maxX, maxY, minZ, maxU, minV);
		rt.addVertexWithUV(minX, maxY, minZ, minU, minV);
		
		// half an actual texel
		double uOffset = 0.5 / tex.getSheetWidth();
		double vOffset = 0.5 / tex.getSheetHeight();
		
		rt.addVertexWithUV(maxX, maxY, minZ, maxU, minV + vOffset);
		rt.addVertexWithUV(maxX, minY, minZ, maxU, minV + vOffset);
		rt.addVertexWithUV(minX, minY, minZ, minU, minV + vOffset);
		rt.addVertexWithUV(minX, maxY, minZ, minU, minV + vOffset);
		
		rt.addVertexWithUV(minX, maxY, maxZ, minU, maxV - vOffset);
		rt.addVertexWithUV(minX, minY, maxZ, minU, maxV - vOffset);
		rt.addVertexWithUV(maxX, minY, maxZ, maxU, maxV - vOffset);
		rt.addVertexWithUV(maxX, maxY, maxZ, maxU, maxV - vOffset);
		
		rt.addVertexWithUV(minX, maxY, minZ, minU + uOffset, minV);
		rt.addVertexWithUV(minX, minY, minZ, minU + uOffset, minV);
		rt.addVertexWithUV(minX, minY, maxZ, minU + uOffset, maxV);
		rt.addVertexWithUV(minX, maxY, maxZ, minU + uOffset, maxV);
		
		rt.addVertexWithUV(maxX, maxY, maxZ, maxU - uOffset, maxV);
		rt.addVertexWithUV(maxX, minY, maxZ, maxU - uOffset, maxV);
		rt.addVertexWithUV(maxX, minY, minZ, maxU - uOffset, minV);
		rt.addVertexWithUV(maxX, maxY, minZ, maxU - uOffset, minV);
		
	}
}
