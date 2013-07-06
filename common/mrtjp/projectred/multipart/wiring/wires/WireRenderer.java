package mrtjp.projectred.multipart.wiring.wires;

import mrtjp.projectred.multipart.wiring.RotatedTessellator;
import mrtjp.projectred.renderstuffs.RenderIDs;
import mrtjp.projectred.utils.BasicRenderUtils;
import mrtjp.projectred.utils.BasicWireUtils;
import mrtjp.projectred.utils.Dir;
import net.minecraft.block.Block;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.Icon;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.common.ForgeDirection;
import cpw.mods.fml.client.registry.ISimpleBlockRenderingHandler;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public class WireRenderer implements ISimpleBlockRenderingHandler {

	private RotatedTessellator rt = new RotatedTessellator();

	public final static WireRenderer instance = new WireRenderer();

	private final static boolean FAT_JACKETED_WIRE = false;

	private final static float SHADE_PY = 1.0f;
	private final static float SHADE_Z = 0.8f;
	private final static float SHADE_X = 0.6f;
	private final static float SHADE_NY = 0.5f;

	private int baseColour;

	public void renderWorld(RenderBlocks render, EnumWire type, TileWire wt, int sideMask, boolean renderJacketed) {

		int x = wt.xCoord, y = wt.yCoord, z = wt.zCoord;

		rt.base = Tessellator.instance;
		rt.flipped = false;
		rt.x = x;
		rt.y = y;
		rt.z = z;

		BasicRenderUtils.setBrightness(wt.worldObj, x, y, z);
		rt.base.setColorOpaque_I(baseColour = wt.getVisualWireColour());

		for (int side = 0; side < 6; side++) {
			if ((sideMask & (1 << side)) == 0)
				continue;

			rt.front = side < 2 ? 2 : 0; // anything not parallel to side
			rt.side = side;

			// System.out.println("rendering side "+side);

			int[] dirMap = BasicWireUtils.dirMap[rt.side][rt.front];

			boolean front = wt.connectsInDirection(side, dirMap[BasicWireUtils.FRONT]);
			boolean back = wt.connectsInDirection(side, dirMap[BasicWireUtils.BACK]);
			boolean left = wt.connectsInDirection(side, dirMap[BasicWireUtils.LEFT]);
			boolean right = wt.connectsInDirection(side, dirMap[BasicWireUtils.RIGHT]);

			boolean frontCorner = front && wt.connectsInDirectionAroundCorner(side, dirMap[BasicWireUtils.FRONT]);
			boolean backCorner = back && wt.connectsInDirectionAroundCorner(side, dirMap[BasicWireUtils.BACK]);
			boolean leftCorner = left && wt.connectsInDirectionAroundCorner(side, dirMap[BasicWireUtils.LEFT]);
			boolean rightCorner = right && wt.connectsInDirectionAroundCorner(side, dirMap[BasicWireUtils.RIGHT]);

			boolean forceEndCaps = true; // TODO optimize if necessary

			EnumWire frontCornerType = frontCorner ? getWireTypeAroundCorner(side, dirMap[BasicWireUtils.FRONT], wt) : null;
			EnumWire backCornerType = backCorner ? getWireTypeAroundCorner(side, dirMap[BasicWireUtils.BACK], wt) : null;
			EnumWire leftCornerType = leftCorner ? getWireTypeAroundCorner(side, dirMap[BasicWireUtils.LEFT], wt) : null;
			EnumWire rightCornerType = rightCorner ? getWireTypeAroundCorner(side, dirMap[BasicWireUtils.RIGHT], wt) : null;

			renderWireSide(rt, render, type, front, back, left, right, frontCorner, backCorner, leftCorner, rightCorner, frontCornerType, backCornerType, leftCornerType, rightCornerType, forceEndCaps, wt.hasJacketedWire());
		}

		if (renderJacketed && wt.hasJacketedWire()) {
			int dirs = wt.getJacketedWireConnectionMask();
			renderWireJacketed(render, type, sideMask, (dirs & (1 << Dir.NX)) != 0, (dirs & (1 << Dir.PX)) != 0, (dirs & (1 << Dir.NY)) != 0, (dirs & (1 << Dir.PY)) != 0, (dirs & (1 << Dir.NZ)) != 0, (dirs & (1 << Dir.PZ)) != 0);
		}
	}

	private EnumWire getWireTypeAroundCorner(int side, int dir, TileWire from) {
		ForgeDirection d1 = ForgeDirection.VALID_DIRECTIONS[side];
		ForgeDirection d2 = ForgeDirection.VALID_DIRECTIONS[dir];
		int x = from.xCoord + d1.offsetX + d2.offsetX;
		int y = from.yCoord + d1.offsetY + d2.offsetY;
		int z = from.zCoord + d1.offsetZ + d2.offsetZ;
		TileEntity te = from.worldObj.getBlockTileEntity(x, y, z);
		if (te instanceof TileWire)
			return ((TileWire) te).getType();
		else
			return null;
	}

	@Override
	public boolean renderWorldBlock(IBlockAccess world, int x, int y, int z, Block block, int modelId, RenderBlocks render) {
		TileWire wt = (TileWire) world.getBlockTileEntity(x, y, z);
		EnumWire type = wt.getType();
		// System.out.println("render wire with type "+type+", sidemask "+wt.getSideMask());
		if (type == null)
			return false;

		byte sideMask = wt.getSideMask();

		renderWorld(render, type, wt, sideMask, wt.hasJacketedWire());

		return true;
	}

	@Override
	public void renderInventoryBlock(Block block, int damageValue, int modelID, RenderBlocks render) {
		EnumWire type = WireDamageValues.getType(damageValue);
		if (type == null)
			return;

		Minecraft.getMinecraft().renderEngine.bindTexture("/terrain.png");

		rt.base = Tessellator.instance;
		rt.flipped = false;
		rt.x = -0.5;
		rt.y = 0;
		rt.z = -0.5;
		rt.front = 2; // anything not parallel to side
		rt.side = Dir.NY;
		rt.base.startDrawingQuads();
		rt.base.setBrightness(0x00F000F0);
		rt.base.setColorOpaque_I(type.itemColour);
		baseColour = type.itemColour;
		if (WireDamageValues.isJacketed(damageValue)) {
			rt.y = -0.5;
			renderWireJacketed(render, type, 0, false, false, true, true, false, false);
		} else
			renderWireSide(rt, render, type, true, true, true, true, false, false, false, false, null, null, null, null, true, false);
		rt.base.draw();
	}

	private void renderWireJacketed(RenderBlocks render, EnumWire type, int sideMask, boolean nx, boolean px, boolean ny, boolean py, boolean nz, boolean pz) {
		Icon tex;

		// no rotation
		rt.side = Dir.NY;
		rt.front = Dir.NZ;

		double _25 = FAT_JACKETED_WIRE ? 0.25 : (1 - type.width) / 2.0;
		double _75 = FAT_JACKETED_WIRE ? 0.75 : (1 + type.width) / 2.0;

		double maxWithWire = 1 - type.thickness;
		double minWithWire = type.thickness;

		int widthPixels = (int) (type.width * 16 + 0.5);
		int _4 = 8 - widthPixels / 2;
		int _12 = 8 + widthPixels / 2;

		if (ny || py || (!nx && !ny && !nz && !px && !py && !pz)) {
			double minY = (sideMask & (1 << Dir.NY)) != 0 ? minWithWire : ny ? 0 : _25;
			double maxY = (sideMask & (1 << Dir.PY)) != 0 ? maxWithWire : py ? 1 : _75;

			tex = (nz || pz) ? type.texture_jacketed_cross : type.texture_jacketed;

			if (render.hasOverrideBlockTexture())
				tex = render.overrideBlockTexture;

			double minV = tex.getInterpolatedV((1 - maxY) * 16);
			double maxV = tex.getInterpolatedV((1 - minY) * 16);

			setNormal(-1, 0, 0);
			rt.addVertexWithUV(_25, minY, _25, tex.getInterpolatedU(_4), maxV);
			rt.addVertexWithUV(_25, minY, _75, tex.getInterpolatedU(_12), maxV);
			rt.addVertexWithUV(_25, maxY, _75, tex.getInterpolatedU(_12), minV);
			rt.addVertexWithUV(_25, maxY, _25, tex.getInterpolatedU(_4), minV);

			setNormal(1, 0, 0);
			rt.addVertexWithUV(_75, maxY, _25, tex.getInterpolatedU(_12), minV);
			rt.addVertexWithUV(_75, maxY, _75, tex.getInterpolatedU(_4), minV);
			rt.addVertexWithUV(_75, minY, _75, tex.getInterpolatedU(_4), maxV);
			rt.addVertexWithUV(_75, minY, _25, tex.getInterpolatedU(_12), maxV);

			tex = (nx || px) ? type.texture_jacketed_cross : type.texture_jacketed;
			if (render.hasOverrideBlockTexture())
				tex = render.overrideBlockTexture;
			minV = tex.getInterpolatedV((1 - maxY) * 16);
			maxV = tex.getInterpolatedV((1 - minY) * 16);

			setNormal(0, 0, -1);
			rt.addVertexWithUV(_25, maxY, _25, tex.getInterpolatedU(_12), minV);
			rt.addVertexWithUV(_75, maxY, _25, tex.getInterpolatedU(_4), minV);
			rt.addVertexWithUV(_75, minY, _25, tex.getInterpolatedU(_4), maxV);
			rt.addVertexWithUV(_25, minY, _25, tex.getInterpolatedU(_12), maxV);

			setNormal(0, 0, 1);
			rt.addVertexWithUV(_25, minY, _75, tex.getInterpolatedU(_4), maxV);
			rt.addVertexWithUV(_75, minY, _75, tex.getInterpolatedU(_12), maxV);
			rt.addVertexWithUV(_75, maxY, _75, tex.getInterpolatedU(_12), minV);
			rt.addVertexWithUV(_25, maxY, _75, tex.getInterpolatedU(_4), minV);

			if (ny || !(nx || px || nz || pz)) {
				tex = type.texture_jacketed_end;

				if (render.hasOverrideBlockTexture())
					tex = render.overrideBlockTexture;

				setNormal(0, -1, 0);
				rt.addVertexWithUV(_75, minY, _25, tex.getInterpolatedU(_12), tex.getInterpolatedV(_4));
				rt.addVertexWithUV(_75, minY, _75, tex.getInterpolatedU(_12), tex.getInterpolatedV(_12));
				rt.addVertexWithUV(_25, minY, _75, tex.getInterpolatedU(_4), tex.getInterpolatedV(_12));
				rt.addVertexWithUV(_25, minY, _25, tex.getInterpolatedU(_4), tex.getInterpolatedV(_4));

			}

			if (py || !(nx || px || nz || pz)) {
				tex = type.texture_jacketed_end;

				if (render.hasOverrideBlockTexture())
					tex = render.overrideBlockTexture;

				setNormal(0, 1, 0);
				rt.addVertexWithUV(_25, maxY, _25, tex.getInterpolatedU(_4), tex.getInterpolatedV(_4));
				rt.addVertexWithUV(_25, maxY, _75, tex.getInterpolatedU(_4), tex.getInterpolatedV(_12));
				rt.addVertexWithUV(_75, maxY, _75, tex.getInterpolatedU(_12), tex.getInterpolatedV(_12));
				rt.addVertexWithUV(_75, maxY, _25, tex.getInterpolatedU(_12), tex.getInterpolatedV(_4));

			}
		}

		if (nx || px) {
			double minX = (sideMask & (1 << Dir.NX)) != 0 ? minWithWire : nx ? 0 : _25;
			double maxX = (sideMask & (1 << Dir.PX)) != 0 ? maxWithWire : px ? 1 : _75;

			tex = (nz || pz) ? type.texture_jacketed_cross : type.texture_jacketed;

			if (render.hasOverrideBlockTexture())
				tex = render.overrideBlockTexture;

			double minV = tex.getInterpolatedV((1 - maxX) * 16);
			double maxV = tex.getInterpolatedV((1 - minX) * 16);

			setNormal(0, -1, 0);
			rt.addVertexWithUV(maxX, _25, _25, tex.getInterpolatedU(_4), minV);
			rt.addVertexWithUV(maxX, _25, _75, tex.getInterpolatedU(_12), minV);
			rt.addVertexWithUV(minX, _25, _75, tex.getInterpolatedU(_12), maxV);
			rt.addVertexWithUV(minX, _25, _25, tex.getInterpolatedU(_4), maxV);

			setNormal(0, 1, 0);
			rt.addVertexWithUV(minX, _75, _25, tex.getInterpolatedU(_12), maxV);
			rt.addVertexWithUV(minX, _75, _75, tex.getInterpolatedU(_4), maxV);
			rt.addVertexWithUV(maxX, _75, _75, tex.getInterpolatedU(_4), minV);
			rt.addVertexWithUV(maxX, _75, _25, tex.getInterpolatedU(_12), minV);

			tex = (ny || py) ? type.texture_jacketed_cross : type.texture_jacketed;
			if (render.hasOverrideBlockTexture())
				tex = render.overrideBlockTexture;
			minV = tex.getInterpolatedV((1 - maxX) * 16);
			maxV = tex.getInterpolatedV((1 - minX) * 16);

			setNormal(0, 0, -1);
			rt.addVertexWithUV(minX, _25, _25, tex.getInterpolatedU(_12), maxV);
			rt.addVertexWithUV(minX, _75, _25, tex.getInterpolatedU(_4), maxV);
			rt.addVertexWithUV(maxX, _75, _25, tex.getInterpolatedU(_4), minV);
			rt.addVertexWithUV(maxX, _25, _25, tex.getInterpolatedU(_12), minV);

			setNormal(0, 0, 1);
			rt.addVertexWithUV(maxX, _25, _75, tex.getInterpolatedU(_4), minV);
			rt.addVertexWithUV(maxX, _75, _75, tex.getInterpolatedU(_12), minV);
			rt.addVertexWithUV(minX, _75, _75, tex.getInterpolatedU(_12), maxV);
			rt.addVertexWithUV(minX, _25, _75, tex.getInterpolatedU(_4), maxV);

			if (nx || !(ny || py || nz || pz)) {
				tex = type.texture_jacketed_end;

				if (render.hasOverrideBlockTexture())
					tex = render.overrideBlockTexture;

				setNormal(-1, 0, 0);
				rt.addVertexWithUV(minX, _25, _25, tex.getInterpolatedU(_4), tex.getInterpolatedV(_4));
				rt.addVertexWithUV(minX, _25, _75, tex.getInterpolatedU(_4), tex.getInterpolatedV(_12));
				rt.addVertexWithUV(minX, _75, _75, tex.getInterpolatedU(_12), tex.getInterpolatedV(_12));
				rt.addVertexWithUV(minX, _75, _25, tex.getInterpolatedU(_12), tex.getInterpolatedV(_4));

			}

			if (px || !(ny || py || nz || pz)) {
				tex = type.texture_jacketed_end;

				if (render.hasOverrideBlockTexture())
					tex = render.overrideBlockTexture;

				setNormal(1, 0, 0);
				rt.addVertexWithUV(maxX, _75, _25, tex.getInterpolatedU(_12), tex.getInterpolatedV(_4));
				rt.addVertexWithUV(maxX, _75, _75, tex.getInterpolatedU(_12), tex.getInterpolatedV(_12));
				rt.addVertexWithUV(maxX, _25, _75, tex.getInterpolatedU(_4), tex.getInterpolatedV(_12));
				rt.addVertexWithUV(maxX, _25, _25, tex.getInterpolatedU(_4), tex.getInterpolatedV(_4));

			}
		}

		if (nz || pz) {
			double minZ = (sideMask & (1 << Dir.NZ)) != 0 ? minWithWire : nz ? 0 : _25;
			double maxZ = (sideMask & (1 << Dir.PZ)) != 0 ? maxWithWire : pz ? 1 : _75;

			tex = (ny || py) ? type.texture_jacketed_cross : type.texture_jacketed;

			if (render.hasOverrideBlockTexture())
				tex = render.overrideBlockTexture;

			double minV = tex.getInterpolatedV((1 - maxZ) * 16);
			double maxV = tex.getInterpolatedV((1 - minZ) * 16);

			setNormal(-1, 0, 0);
			rt.addVertexWithUV(_25, _25, maxZ, tex.getInterpolatedU(_4), minV);
			rt.addVertexWithUV(_25, _75, maxZ, tex.getInterpolatedU(_12), minV);
			rt.addVertexWithUV(_25, _75, minZ, tex.getInterpolatedU(_12), maxV);
			rt.addVertexWithUV(_25, _25, minZ, tex.getInterpolatedU(_4), maxV);

			setNormal(1, 0, 0);
			rt.addVertexWithUV(_75, _25, minZ, tex.getInterpolatedU(_12), maxV);
			rt.addVertexWithUV(_75, _75, minZ, tex.getInterpolatedU(_4), maxV);
			rt.addVertexWithUV(_75, _75, maxZ, tex.getInterpolatedU(_4), minV);
			rt.addVertexWithUV(_75, _25, maxZ, tex.getInterpolatedU(_12), minV);

			tex = (nx || px) ? type.texture_jacketed_cross : type.texture_jacketed;
			if (render.hasOverrideBlockTexture())
				tex = render.overrideBlockTexture;
			minV = tex.getInterpolatedV((1 - maxZ) * 16);
			maxV = tex.getInterpolatedV((1 - minZ) * 16);

			setNormal(0, -1, 0);
			rt.addVertexWithUV(_25, _25, minZ, tex.getInterpolatedU(_12), maxV);
			rt.addVertexWithUV(_75, _25, minZ, tex.getInterpolatedU(_4), maxV);
			rt.addVertexWithUV(_75, _25, maxZ, tex.getInterpolatedU(_4), minV);
			rt.addVertexWithUV(_25, _25, maxZ, tex.getInterpolatedU(_12), minV);

			setNormal(0, 1, 0);
			rt.addVertexWithUV(_25, _75, maxZ, tex.getInterpolatedU(_4), minV);
			rt.addVertexWithUV(_75, _75, maxZ, tex.getInterpolatedU(_12), minV);
			rt.addVertexWithUV(_75, _75, minZ, tex.getInterpolatedU(_12), maxV);
			rt.addVertexWithUV(_25, _75, minZ, tex.getInterpolatedU(_4), maxV);

			if (nz || !(nx || px || ny || py)) {
				tex = type.texture_jacketed_end;

				if (render.hasOverrideBlockTexture())
					tex = render.overrideBlockTexture;

				setNormal(0, 0, -1);
				rt.addVertexWithUV(_25, _25, minZ, tex.getInterpolatedU(_4), tex.getInterpolatedV(_4));
				rt.addVertexWithUV(_25, _75, minZ, tex.getInterpolatedU(_4), tex.getInterpolatedV(_12));
				rt.addVertexWithUV(_75, _75, minZ, tex.getInterpolatedU(_12), tex.getInterpolatedV(_12));
				rt.addVertexWithUV(_75, _25, minZ, tex.getInterpolatedU(_12), tex.getInterpolatedV(_4));

			}

			if (pz || !(nx || px || ny || py)) {
				tex = type.texture_jacketed_end;

				if (render.hasOverrideBlockTexture())
					tex = render.overrideBlockTexture;

				setNormal(0, 0, 1);
				rt.addVertexWithUV(_75, _25, maxZ, tex.getInterpolatedU(_12), tex.getInterpolatedV(_4));
				rt.addVertexWithUV(_75, _75, maxZ, tex.getInterpolatedU(_12), tex.getInterpolatedV(_12));
				rt.addVertexWithUV(_25, _75, maxZ, tex.getInterpolatedU(_4), tex.getInterpolatedV(_12));
				rt.addVertexWithUV(_25, _25, maxZ, tex.getInterpolatedU(_4), tex.getInterpolatedV(_4));

			}
		}

	}

	private void setNormal(float x, float y, float z) {
		rt.base.setNormal(x, y, z);

		float r = ((baseColour >> 16) & 255) / 255.0f;
		float g = ((baseColour >> 8) & 255) / 255.0f;
		float b = (baseColour & 255) / 255.0f;

		if (y > 0)
			rt.base.setColorOpaque_F(r * SHADE_PY, g * SHADE_PY, b * SHADE_PY);
		else if (x != 0)
			rt.base.setColorOpaque_F(r * SHADE_X, g * SHADE_X, b * SHADE_X);
		else if (z != 0)
			rt.base.setColorOpaque_F(r * SHADE_Z, g * SHADE_Z, b * SHADE_Z);
		else
			// y < 0
			rt.base.setColorOpaque_F(r * SHADE_NY, g * SHADE_NY, b * SHADE_NY);
	}

	public static boolean OLD_CORNER_SIDES = false;

	public static void renderWireSide(RotatedTessellator rt, RenderBlocks render, EnumWire type, boolean nz, boolean pz, boolean nx, boolean px, boolean nzCorner, boolean pzCorner, boolean nxCorner, boolean pxCorner, EnumWire nzCornerType, EnumWire pzCornerType, EnumWire nxCornerType, EnumWire pxCornerType, boolean forceEndCaps, boolean haveJacketed) {
		double thick = type.thickness;
		double w = type.width / 2;
		double W = type.width * 16 / 2;

		boolean unconnected = !nz && !pz && !nx && !px;

		final int NZ = 8;
		final int PZ = 4;
		final int NX = 2;
		final int PX = 1;

		Icon tex = type.texture_cross;

		switch ((nz ? NZ : 0) | (pz ? PZ : 0) | (nx ? NX : 0) | (px ? PX : 0)) {
		case 0:
			tex = type.texture_none;
			break;
		case PX:
			tex = type.texture_end_nx;
			break;
		case NX:
			tex = type.texture_end_px;
			break;
		case PZ:
			tex = type.texture_end_nz;
			break;
		case NZ:
			tex = type.texture_end_pz;
			break;
		case PX | NX:
			tex = type.texture_straight_x;
			break;
		case PZ | NZ:
			tex = type.texture_straight_z;
			break;
		case NX | NZ:
			tex = type.texture_corner_nn;
			break;
		case PX | NZ:
			tex = type.texture_corner_pn;
			break;
		case NX | PZ:
			tex = type.texture_corner_np;
			break;
		case PX | PZ:
			tex = type.texture_corner_pp;
			break;
		case NX | PX | NZ:
			tex = type.texture_tee_nz;
			break;
		case NX | PX | PZ:
			tex = type.texture_tee_pz;
			break;
		case NZ | PZ | NX:
			tex = type.texture_tee_nx;
			break;
		case NZ | PZ | PX:
			tex = type.texture_tee_px;
			break;
		case PX | NX | PZ | NZ:
			tex = type.texture_cross;
			break;
		}

		if (render.hasOverrideBlockTexture())
			tex = render.overrideBlockTexture;

		double minX = nx ? 0 : px && !nz && !pz ? 0.25 : 0.5 - w;
		double maxX = px ? 1 : nx && !nz && !pz ? 0.75 : 0.5 + w;
		double minZ = nz ? 0 : unconnected || (pz && !nx && !px) ? 0.25 : 0.5 - w;
		double maxZ = pz ? 1 : unconnected || (nz && !nx && !px) ? 0.75 : 0.5 + w;

		// otherwise the wire sticks out under jacketed wire if less than 8
		// pixels wide
		// and looks ugly
		if (haveJacketed) {
			if (pz && !nz && !nx && !px)
				minZ = 0.5 - type.width / 2;
			if (nz && !pz && !nx && !px)
				maxZ = 0.5 + type.width / 2;
			if (px && !nx && !nz && !pz)
				minX = 0.5 - type.width / 2;
			if (nx && !px && !nz && !pz)
				maxX = 0.5 + type.width / 2;
		}

		// if(nxCorner) minX -= thick;
		// if(pxCorner) maxX += thick;
		// if(nzCorner) minZ -= thick;
		// if(pzCorner) maxZ += thick;

		// +/- Z
		if (nz || pz || unconnected) {
			Icon sideTex = type.texture_straight_z;
			Icon endTex = type.texture_straight_x;

			if (render.hasOverrideBlockTexture())
				sideTex = endTex = render.overrideBlockTexture;

			rt.base.setNormal(0, 1, 0);
			rt.addVertexWithUV(0.5 - w, thick, minZ, tex.getInterpolatedU(8 - W), tex.getInterpolatedV(minZ * 16));
			rt.addVertexWithUV(0.5 - w, thick, maxZ, tex.getInterpolatedU(8 - W), tex.getInterpolatedV(maxZ * 16));
			rt.addVertexWithUV(0.5 + w, thick, maxZ, tex.getInterpolatedU(8 + W), tex.getInterpolatedV(maxZ * 16));
			rt.addVertexWithUV(0.5 + w, thick, minZ, tex.getInterpolatedU(8 + W), tex.getInterpolatedV(minZ * 16));

			rt.base.setNormal(-1, 0, 0);
			rt.addVertexWithUV(0.5 - w, 0, minZ, sideTex.getInterpolatedU(8 - W + thick * 16), sideTex.getInterpolatedV(minZ * 16));
			rt.addVertexWithUV(0.5 - w, 0, maxZ, sideTex.getInterpolatedU(8 - W + thick * 16), sideTex.getInterpolatedV(maxZ * 16));
			rt.addVertexWithUV(0.5 - w, thick, maxZ, sideTex.getInterpolatedU(8 - W), sideTex.getInterpolatedV(maxZ * 16));
			rt.addVertexWithUV(0.5 - w, thick, minZ, sideTex.getInterpolatedU(8 - W), sideTex.getInterpolatedV(minZ * 16));

			rt.base.setNormal(1, 0, 0);
			rt.addVertexWithUV(0.5 + w, 0, maxZ, sideTex.getInterpolatedU(8 - W + thick * 16), sideTex.getInterpolatedV(maxZ * 16));
			rt.addVertexWithUV(0.5 + w, 0, minZ, sideTex.getInterpolatedU(8 - W + thick * 16), sideTex.getInterpolatedV(minZ * 16));
			rt.addVertexWithUV(0.5 + w, thick, minZ, sideTex.getInterpolatedU(8 - W), sideTex.getInterpolatedV(minZ * 16));
			rt.addVertexWithUV(0.5 + w, thick, maxZ, sideTex.getInterpolatedU(8 - W), sideTex.getInterpolatedV(maxZ * 16));

			if (nzCorner) {
				double cornerMaxZ = minZ;
				minZ -= thick;

				Icon cornerSideTex = type.texture_corner_nn;

				if (render.hasOverrideBlockTexture())
					cornerSideTex = render.overrideBlockTexture;

				rt.base.setNormal(0, 1, 0);
				rt.addVertexWithUV(0.5 - w, thick, minZ, tex.getInterpolatedU(8 - W), tex.getInterpolatedV(0));
				rt.addVertexWithUV(0.5 - w, thick, cornerMaxZ, tex.getInterpolatedU(8 - W), tex.getInterpolatedV(thick * 16));
				rt.addVertexWithUV(0.5 + w, thick, cornerMaxZ, tex.getInterpolatedU(8 + W), tex.getInterpolatedV(thick * 16));
				rt.addVertexWithUV(0.5 + w, thick, minZ, tex.getInterpolatedU(8 + W), tex.getInterpolatedV(0));

				if (nzCornerType == null || nzCornerType.thickness < type.thickness || nzCornerType.width < type.width) {
					rt.base.setNormal(0, -1, 0);
					rt.addVertexWithUV(0.5 + w, 0, minZ, tex.getInterpolatedU(8 + W), tex.getInterpolatedV(0));
					rt.addVertexWithUV(0.5 + w, 0, cornerMaxZ, tex.getInterpolatedU(8 + W), tex.getInterpolatedV(thick * 16));
					rt.addVertexWithUV(0.5 - w, 0, cornerMaxZ, tex.getInterpolatedU(8 - W), tex.getInterpolatedV(thick * 16));
					rt.addVertexWithUV(0.5 - w, 0, minZ, tex.getInterpolatedU(8 - W), tex.getInterpolatedV(0));

				}

				if (nzCornerType != null && nzCornerType != type) {
					cornerSideTex = sideTex;

					rt.base.setNormal(-1, 0, 0);
					rt.addVertexWithUV(0.5 - w, 0, minZ, cornerSideTex.getInterpolatedU(8 - W + thick * 16), cornerSideTex.getInterpolatedV(8 + W));
					rt.addVertexWithUV(0.5 - w, 0, cornerMaxZ, cornerSideTex.getInterpolatedU(8 - W + thick * 16), cornerSideTex.getInterpolatedV(8 + W - thick * 16));
					rt.addVertexWithUV(0.5 - w, thick, cornerMaxZ, cornerSideTex.getInterpolatedU(8 - W), cornerSideTex.getInterpolatedV(8 + W - thick * 16));
					rt.addVertexWithUV(0.5 - w, thick, minZ, cornerSideTex.getInterpolatedU(8 - W), cornerSideTex.getInterpolatedV(8 + W));

					rt.base.setNormal(1, 0, 0);
					rt.addVertexWithUV(0.5 + w, 0, cornerMaxZ, cornerSideTex.getInterpolatedU(8 - W + thick * 16), cornerSideTex.getInterpolatedV(8 + W - thick * 16));
					rt.addVertexWithUV(0.5 + w, 0, minZ, cornerSideTex.getInterpolatedU(8 - W + thick * 16), cornerSideTex.getInterpolatedV(8 + W));
					rt.addVertexWithUV(0.5 + w, thick, minZ, cornerSideTex.getInterpolatedU(8 - W), cornerSideTex.getInterpolatedV(8 + W));
					rt.addVertexWithUV(0.5 + w, thick, cornerMaxZ, cornerSideTex.getInterpolatedU(8 - W), cornerSideTex.getInterpolatedV(8 + W - thick * 16));

				} else {
					cornerSideTex = type.texture_none;

					if (render.hasOverrideBlockTexture())
						cornerSideTex = render.overrideBlockTexture;

					rt.base.setNormal(-1, 0, 0);
					rt.addVertexWithUV(0.5 - w, 0, minZ, cornerSideTex.getInterpolatedU(thick * 16), cornerSideTex.getInterpolatedV(0));
					rt.addVertexWithUV(0.5 - w, 0, cornerMaxZ, cornerSideTex.getInterpolatedU(thick * 16), cornerSideTex.getInterpolatedV(thick * 16));
					rt.addVertexWithUV(0.5 - w, thick, cornerMaxZ, cornerSideTex.getInterpolatedU(0), cornerSideTex.getInterpolatedV(thick * 16));
					rt.addVertexWithUV(0.5 - w, thick, minZ, cornerSideTex.getInterpolatedU(0), cornerSideTex.getInterpolatedV(0));

					rt.base.setNormal(1, 0, 0);
					rt.addVertexWithUV(0.5 + w, 0, cornerMaxZ, cornerSideTex.getInterpolatedU(thick * 16), cornerSideTex.getInterpolatedV(thick * 16));
					rt.addVertexWithUV(0.5 + w, 0, minZ, cornerSideTex.getInterpolatedU(thick * 16), cornerSideTex.getInterpolatedV(0));
					rt.addVertexWithUV(0.5 + w, thick, minZ, cornerSideTex.getInterpolatedU(0), cornerSideTex.getInterpolatedV(0));
					rt.addVertexWithUV(0.5 + w, thick, cornerMaxZ, cornerSideTex.getInterpolatedU(0), cornerSideTex.getInterpolatedV(thick * 16));
				}
			}

			if (pzCorner) {
				double cornerMinZ = maxZ;
				maxZ += thick;

				Icon cornerSideTex = type.texture_corner_np;

				if (render.hasOverrideBlockTexture())
					cornerSideTex = render.overrideBlockTexture;

				// top
				rt.base.setNormal(0, 1, 0);
				rt.addVertexWithUV(0.5 - w, thick, cornerMinZ, tex.getInterpolatedU(8 - W), tex.getInterpolatedV((1 - thick) * 16));
				rt.addVertexWithUV(0.5 - w, thick, maxZ, tex.getInterpolatedU(8 - W), tex.getInterpolatedV(16));
				rt.addVertexWithUV(0.5 + w, thick, maxZ, tex.getInterpolatedU(8 + W), tex.getInterpolatedV(16));
				rt.addVertexWithUV(0.5 + w, thick, cornerMinZ, tex.getInterpolatedU(8 + W), tex.getInterpolatedV((1 - thick) * 16));

				if (pzCornerType == null || pzCornerType.thickness < type.thickness || pzCornerType.width < type.width) {
					rt.base.setNormal(0, -1, 0);
					rt.addVertexWithUV(0.5 + w, 0, cornerMinZ, tex.getInterpolatedU(8 + W), tex.getInterpolatedV((1 - thick) * 16));
					rt.addVertexWithUV(0.5 + w, 0, maxZ, tex.getInterpolatedU(8 + W), tex.getInterpolatedV(16));
					rt.addVertexWithUV(0.5 - w, 0, maxZ, tex.getInterpolatedU(8 - W), tex.getInterpolatedV(16));
					rt.addVertexWithUV(0.5 - w, 0, cornerMinZ, tex.getInterpolatedU(8 - W), tex.getInterpolatedV((1 - thick) * 16));

				}

				if (pzCornerType != null && pzCornerType != type) {
					cornerSideTex = sideTex;

					rt.base.setNormal(-1, 0, 0);
					rt.addVertexWithUV(0.5 - w, 0, cornerMinZ, cornerSideTex.getInterpolatedU(8 - W + thick * 16), cornerSideTex.getInterpolatedV(8 - W + thick * 16));
					rt.addVertexWithUV(0.5 - w, 0, maxZ, cornerSideTex.getInterpolatedU(8 - W + thick * 16), cornerSideTex.getInterpolatedV(8 - W));
					rt.addVertexWithUV(0.5 - w, thick, maxZ, cornerSideTex.getInterpolatedU(8 - W), cornerSideTex.getInterpolatedV(8 - W));
					rt.addVertexWithUV(0.5 - w, thick, cornerMinZ, cornerSideTex.getInterpolatedU(8 - W), cornerSideTex.getInterpolatedV(8 - W + thick * 16));

					rt.base.setNormal(1, 0, 0);
					rt.addVertexWithUV(0.5 + w, 0, maxZ, cornerSideTex.getInterpolatedU(8 - W + thick * 16), cornerSideTex.getInterpolatedV(8 - W));
					rt.addVertexWithUV(0.5 + w, 0, cornerMinZ, cornerSideTex.getInterpolatedU(8 - W + thick * 16), cornerSideTex.getInterpolatedV(8 - W + thick * 16));
					rt.addVertexWithUV(0.5 + w, thick, cornerMinZ, cornerSideTex.getInterpolatedU(8 - W), cornerSideTex.getInterpolatedV(8 - W + thick * 16));
					rt.addVertexWithUV(0.5 + w, thick, maxZ, cornerSideTex.getInterpolatedU(8 - W), cornerSideTex.getInterpolatedV(8 - W));

				} else {
					cornerSideTex = type.texture_none;

					if (render.hasOverrideBlockTexture())
						cornerSideTex = render.overrideBlockTexture;

					rt.base.setNormal(-1, 0, 0);
					rt.addVertexWithUV(0.5 - w, 0, cornerMinZ, cornerSideTex.getInterpolatedU(thick * 16), cornerSideTex.getInterpolatedV(thick * 16));
					rt.addVertexWithUV(0.5 - w, 0, maxZ, cornerSideTex.getInterpolatedU(0), cornerSideTex.getInterpolatedV(thick * 16));
					rt.addVertexWithUV(0.5 - w, thick, maxZ, cornerSideTex.getInterpolatedU(0), cornerSideTex.getInterpolatedV(0));
					rt.addVertexWithUV(0.5 - w, thick, cornerMinZ, cornerSideTex.getInterpolatedU(thick * 16), cornerSideTex.getInterpolatedV(0));

					rt.base.setNormal(1, 0, 0);
					rt.addVertexWithUV(0.5 + w, 0, maxZ, cornerSideTex.getInterpolatedU(0), cornerSideTex.getInterpolatedV(thick * 16));
					rt.addVertexWithUV(0.5 + w, 0, cornerMinZ, cornerSideTex.getInterpolatedU(thick * 16), cornerSideTex.getInterpolatedV(thick * 16));
					rt.addVertexWithUV(0.5 + w, thick, cornerMinZ, cornerSideTex.getInterpolatedU(thick * 16), cornerSideTex.getInterpolatedV(0));
					rt.addVertexWithUV(0.5 + w, thick, maxZ, cornerSideTex.getInterpolatedU(0), cornerSideTex.getInterpolatedV(0));
				}
			}

			if ((!nz || forceEndCaps) && (!nzCorner || nzCornerType == null || nzCornerType.thickness < type.thickness)) {
				rt.base.setNormal(0, 0, -1);
				rt.addVertexWithUV(0.5 - w, thick, minZ, endTex.getInterpolatedU(8 - W), endTex.getInterpolatedV(8 - W));
				rt.addVertexWithUV(0.5 + w, thick, minZ, endTex.getInterpolatedU(8 + W), endTex.getInterpolatedV(8 - W));
				rt.addVertexWithUV(0.5 + w, 0, minZ, endTex.getInterpolatedU(8 + W), endTex.getInterpolatedV(8 - W + thick * 16));
				rt.addVertexWithUV(0.5 - w, 0, minZ, endTex.getInterpolatedU(8 - W), endTex.getInterpolatedV(8 - W + thick * 16));
			}

			if ((!pz || forceEndCaps) && (!pzCorner || pzCornerType == null || pzCornerType.thickness < type.thickness)) {
				rt.base.setNormal(0, 0, 1);
				rt.addVertexWithUV(0.5 - w, 0, maxZ, endTex.getInterpolatedU(8 - W), endTex.getInterpolatedV(8 - W + thick * 16));
				rt.addVertexWithUV(0.5 + w, 0, maxZ, endTex.getInterpolatedU(8 + W), endTex.getInterpolatedV(8 - W + thick * 16));
				rt.addVertexWithUV(0.5 + w, thick, maxZ, endTex.getInterpolatedU(8 + W), endTex.getInterpolatedV(8 - W));
				rt.addVertexWithUV(0.5 - w, thick, maxZ, endTex.getInterpolatedU(8 - W), endTex.getInterpolatedV(8 - W));
			}
		}

		// +/- X
		if (nx || px) {
			Icon sideTex = type.texture_straight_x;
			Icon endTex = type.texture_straight_z;

			if (render.hasOverrideBlockTexture())
				sideTex = endTex = render.overrideBlockTexture;

			rt.base.setNormal(0, 1, 0);
			rt.addVertexWithUV(minX, thick, 0.5 - w, tex.getInterpolatedU(minX * 16), tex.getInterpolatedV(8 - W));
			rt.addVertexWithUV(minX, thick, 0.5 + w, tex.getInterpolatedU(minX * 16), tex.getInterpolatedV(8 + W));
			rt.addVertexWithUV(maxX, thick, 0.5 + w, tex.getInterpolatedU(maxX * 16), tex.getInterpolatedV(8 + W));
			rt.addVertexWithUV(maxX, thick, 0.5 - w, tex.getInterpolatedU(maxX * 16), tex.getInterpolatedV(8 - W));

			rt.base.setNormal(0, 0, -1);
			rt.addVertexWithUV(maxX, 0, 0.5 - w, sideTex.getInterpolatedU(maxX * 16), sideTex.getInterpolatedV(8 - W + thick * 16));
			rt.addVertexWithUV(minX, 0, 0.5 - w, sideTex.getInterpolatedU(minX * 16), sideTex.getInterpolatedV(8 - W + thick * 16));
			rt.addVertexWithUV(minX, thick, 0.5 - w, sideTex.getInterpolatedU(minX * 16), sideTex.getInterpolatedV(8 - W));
			rt.addVertexWithUV(maxX, thick, 0.5 - w, sideTex.getInterpolatedU(maxX * 16), sideTex.getInterpolatedV(8 - W));

			rt.base.setNormal(0, 0, 1);
			rt.addVertexWithUV(minX, 0, 0.5 + w, sideTex.getInterpolatedU(minX * 16), sideTex.getInterpolatedV(8 - W + thick * 16));
			rt.addVertexWithUV(maxX, 0, 0.5 + w, sideTex.getInterpolatedU(maxX * 16), sideTex.getInterpolatedV(8 - W + thick * 16));
			rt.addVertexWithUV(maxX, thick, 0.5 + w, sideTex.getInterpolatedU(maxX * 16), sideTex.getInterpolatedV(8 - W));
			rt.addVertexWithUV(minX, thick, 0.5 + w, sideTex.getInterpolatedU(minX * 16), sideTex.getInterpolatedV(8 - W));

			if (nxCorner) {
				double cornerMaxX = minX;
				minX -= thick;

				Icon cornerSideTex = type.texture_corner_pn;

				if (render.hasOverrideBlockTexture())
					cornerSideTex = render.overrideBlockTexture;

				rt.base.setNormal(0, 1, 0);
				rt.addVertexWithUV(minX, thick, 0.5 + w, tex.getInterpolatedU(0), tex.getInterpolatedV(8 + W));
				rt.addVertexWithUV(cornerMaxX, thick, 0.5 + w, tex.getInterpolatedU(thick * 16), tex.getInterpolatedV(8 + W));
				rt.addVertexWithUV(cornerMaxX, thick, 0.5 - w, tex.getInterpolatedU(thick * 16), tex.getInterpolatedV(8 - W));
				rt.addVertexWithUV(minX, thick, 0.5 - w, tex.getInterpolatedU(0), tex.getInterpolatedV(8 - W));

				if (nxCornerType == null || nxCornerType.thickness < type.thickness || nxCornerType.width < type.width) {
					rt.base.setNormal(0, -1, 0);
					rt.addVertexWithUV(minX, 0, 0.5 - w, tex.getInterpolatedU(0), tex.getInterpolatedV(8 - W));
					rt.addVertexWithUV(cornerMaxX, 0, 0.5 - w, tex.getInterpolatedU(thick * 16), tex.getInterpolatedV(8 - W));
					rt.addVertexWithUV(cornerMaxX, 0, 0.5 + w, tex.getInterpolatedU(thick * 16), tex.getInterpolatedV(8 + W));
					rt.addVertexWithUV(minX, 0, 0.5 + w, tex.getInterpolatedU(0), tex.getInterpolatedV(8 + W));

				}

				if (nxCornerType != null && nxCornerType != type) {
					cornerSideTex = sideTex;

					rt.base.setNormal(0, 0, -1);
					rt.addVertexWithUV(minX, thick, 0.5 - w, cornerSideTex.getInterpolatedU(8 - W), cornerSideTex.getInterpolatedV(8 - W));
					rt.addVertexWithUV(cornerMaxX, thick, 0.5 - w, cornerSideTex.getInterpolatedU(8 - W + thick * 16), cornerSideTex.getInterpolatedV(8 - W));
					rt.addVertexWithUV(cornerMaxX, 0, 0.5 - w, cornerSideTex.getInterpolatedU(8 - W + thick * 16), cornerSideTex.getInterpolatedV(8 - W + thick * 16));
					rt.addVertexWithUV(minX, 0, 0.5 - w, cornerSideTex.getInterpolatedU(8 - W), cornerSideTex.getInterpolatedV(8 - W + thick * 16));

					rt.base.setNormal(0, 0, 1);
					rt.addVertexWithUV(cornerMaxX, thick, 0.5 + w, cornerSideTex.getInterpolatedU(8 - W + thick * 16), cornerSideTex.getInterpolatedV(8 - W));
					rt.addVertexWithUV(minX, thick, 0.5 + w, cornerSideTex.getInterpolatedU(8 - W), cornerSideTex.getInterpolatedV(8 - W));
					rt.addVertexWithUV(minX, 0, 0.5 + w, cornerSideTex.getInterpolatedU(8 - W), cornerSideTex.getInterpolatedV(8 - W + thick * 16));
					rt.addVertexWithUV(cornerMaxX, 0, 0.5 + w, cornerSideTex.getInterpolatedU(8 - W + thick * 16), cornerSideTex.getInterpolatedV(8 - W + thick * 16));

				} else {
					cornerSideTex = type.texture_none;

					if (render.hasOverrideBlockTexture())
						cornerSideTex = render.overrideBlockTexture;

					rt.base.setNormal(0, 0, -1);
					rt.addVertexWithUV(minX, thick, 0.5 - w, cornerSideTex.getInterpolatedU(0), cornerSideTex.getInterpolatedV(0));
					rt.addVertexWithUV(cornerMaxX, thick, 0.5 - w, cornerSideTex.getInterpolatedU(thick * 16), cornerSideTex.getInterpolatedV(0));
					rt.addVertexWithUV(cornerMaxX, 0, 0.5 - w, cornerSideTex.getInterpolatedU(thick * 16), cornerSideTex.getInterpolatedV(thick * 16));
					rt.addVertexWithUV(minX, 0, 0.5 - w, cornerSideTex.getInterpolatedU(0), cornerSideTex.getInterpolatedV(thick * 16));

					rt.base.setNormal(0, 0, 1);
					rt.addVertexWithUV(cornerMaxX, thick, 0.5 + w, cornerSideTex.getInterpolatedU(thick * 16), cornerSideTex.getInterpolatedV(0));
					rt.addVertexWithUV(minX, thick, 0.5 + w, cornerSideTex.getInterpolatedU(0), cornerSideTex.getInterpolatedV(0));
					rt.addVertexWithUV(minX, 0, 0.5 + w, cornerSideTex.getInterpolatedU(0), cornerSideTex.getInterpolatedV(thick * 16));
					rt.addVertexWithUV(cornerMaxX, 0, 0.5 + w, cornerSideTex.getInterpolatedU(thick * 16), cornerSideTex.getInterpolatedV(thick * 16));
				}
			}

			if (pxCorner) {
				double cornerMinX = maxX;
				maxX += thick;

				Icon cornerSideTex = type.texture_corner_pp;

				if (render.hasOverrideBlockTexture())
					cornerSideTex = render.overrideBlockTexture;

				rt.base.setNormal(0, 1, 0);
				rt.addVertexWithUV(cornerMinX, thick, 0.5 + w, tex.getInterpolatedU((1 - thick) * 16), tex.getInterpolatedV(8 + W));
				rt.addVertexWithUV(maxX, thick, 0.5 + w, tex.getInterpolatedU(16), tex.getInterpolatedV(8 + W));
				rt.addVertexWithUV(maxX, thick, 0.5 - w, tex.getInterpolatedU(16), tex.getInterpolatedV(8 - W));
				rt.addVertexWithUV(cornerMinX, thick, 0.5 - w, tex.getInterpolatedU((1 - thick) * 16), tex.getInterpolatedV(8 - W));

				if (pxCornerType == null || pxCornerType.thickness < type.thickness || pxCornerType.width < type.width) {
					rt.base.setNormal(0, -1, 0);
					rt.addVertexWithUV(cornerMinX, 0, 0.5 - w, tex.getInterpolatedU((1 - thick) * 16), tex.getInterpolatedV(8 - W));
					rt.addVertexWithUV(maxX, 0, 0.5 - w, tex.getInterpolatedU(16), tex.getInterpolatedV(8 - W));
					rt.addVertexWithUV(maxX, 0, 0.5 + w, tex.getInterpolatedU(16), tex.getInterpolatedV(8 + W));
					rt.addVertexWithUV(cornerMinX, 0, 0.5 + w, tex.getInterpolatedU((1 - thick) * 16), tex.getInterpolatedV(8 + W));

				}

				if (pxCornerType != null && pxCornerType != type) {
					cornerSideTex = sideTex;

					rt.base.setNormal(0, 0, -1);
					rt.addVertexWithUV(cornerMinX, thick, 0.5 - w, cornerSideTex.getInterpolatedU(8 - W + thick * 16), cornerSideTex.getInterpolatedV(8 - W));
					rt.addVertexWithUV(maxX, thick, 0.5 - w, cornerSideTex.getInterpolatedU(8 - W), cornerSideTex.getInterpolatedV(8 - W));
					rt.addVertexWithUV(maxX, 0, 0.5 - w, cornerSideTex.getInterpolatedU(8 - W), cornerSideTex.getInterpolatedV(8 - W + thick * 16));
					rt.addVertexWithUV(cornerMinX, 0, 0.5 - w, cornerSideTex.getInterpolatedU(8 - W + thick * 16), cornerSideTex.getInterpolatedV(8 - W + thick * 16));

					rt.base.setNormal(0, 0, 1);
					rt.addVertexWithUV(maxX, thick, 0.5 + w, cornerSideTex.getInterpolatedU(8 - W), cornerSideTex.getInterpolatedV(8 - W));
					rt.addVertexWithUV(cornerMinX, thick, 0.5 + w, cornerSideTex.getInterpolatedU(8 - W + thick * 16), cornerSideTex.getInterpolatedV(8 - W));
					rt.addVertexWithUV(cornerMinX, 0, 0.5 + w, cornerSideTex.getInterpolatedU(8 - W + thick * 16), cornerSideTex.getInterpolatedV(8 - W + thick * 16));
					rt.addVertexWithUV(maxX, 0, 0.5 + w, cornerSideTex.getInterpolatedU(8 - W), cornerSideTex.getInterpolatedV(8 - W + thick * 16));

				} else {
					cornerSideTex = type.texture_none;

					if (render.hasOverrideBlockTexture())
						cornerSideTex = render.overrideBlockTexture;

					rt.base.setNormal(0, 0, -1);
					rt.addVertexWithUV(cornerMinX, thick, 0.5 - w, cornerSideTex.getInterpolatedU(thick * 16), cornerSideTex.getInterpolatedV(0));
					rt.addVertexWithUV(maxX, thick, 0.5 - w, cornerSideTex.getInterpolatedU(0), cornerSideTex.getInterpolatedV(0));
					rt.addVertexWithUV(maxX, 0, 0.5 - w, cornerSideTex.getInterpolatedU(0), cornerSideTex.getInterpolatedV(thick * 16));
					rt.addVertexWithUV(cornerMinX, 0, 0.5 - w, cornerSideTex.getInterpolatedU(thick * 16), cornerSideTex.getInterpolatedV(thick * 16));

					rt.base.setNormal(0, 0, 1);
					rt.addVertexWithUV(maxX, thick, 0.5 + w, cornerSideTex.getInterpolatedU(0), cornerSideTex.getInterpolatedV(0));
					rt.addVertexWithUV(cornerMinX, thick, 0.5 + w, cornerSideTex.getInterpolatedU(thick * 16), cornerSideTex.getInterpolatedV(0));
					rt.addVertexWithUV(cornerMinX, 0, 0.5 + w, cornerSideTex.getInterpolatedU(thick * 16), cornerSideTex.getInterpolatedV(thick * 16));
					rt.addVertexWithUV(maxX, 0, 0.5 + w, cornerSideTex.getInterpolatedU(0), cornerSideTex.getInterpolatedV(thick * 16));
				}
			}

			if ((forceEndCaps || !nx) && (!nxCorner || nxCornerType == null || nxCornerType.thickness < type.thickness)) {
				rt.base.setNormal(0, 0, -1);
				rt.addVertexWithUV(minX, thick, 0.5 + w, endTex.getInterpolatedU(8 - W), endTex.getInterpolatedV(8 + W));
				rt.addVertexWithUV(minX, thick, 0.5 - w, endTex.getInterpolatedU(8 - W), endTex.getInterpolatedV(8 - W));
				rt.addVertexWithUV(minX, 0, 0.5 - w, endTex.getInterpolatedU(8 - W + thick * 16), endTex.getInterpolatedV(8 - W));
				rt.addVertexWithUV(minX, 0, 0.5 + w, endTex.getInterpolatedU(8 - W + thick * 16), endTex.getInterpolatedV(8 + W));
			}

			if ((forceEndCaps || !px) && (!pxCorner || pxCornerType == null || pxCornerType.thickness < type.thickness)) {
				rt.base.setNormal(0, 0, 1);
				rt.addVertexWithUV(maxX, 0, 0.5 + w, endTex.getInterpolatedU(8 - W + thick * 16), endTex.getInterpolatedV(8 + W));
				rt.addVertexWithUV(maxX, 0, 0.5 - w, endTex.getInterpolatedU(8 - W + thick * 16), endTex.getInterpolatedV(8 - W));
				rt.addVertexWithUV(maxX, thick, 0.5 - w, endTex.getInterpolatedU(8 - W), endTex.getInterpolatedV(8 - W));
				rt.addVertexWithUV(maxX, thick, 0.5 + w, endTex.getInterpolatedU(8 - W), endTex.getInterpolatedV(8 + W));
			}
		}

	}


	@Override
	public boolean shouldRender3DInInventory() {
		return true;
	}

	@Override
	public int getRenderId() {
		return RenderIDs.renderIdRedwire;
	}

}
