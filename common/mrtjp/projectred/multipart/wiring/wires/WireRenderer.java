package mrtjp.projectred.multipart.wiring.wires;

import mrtjp.projectred.multipart.wiring.RotatedRenderer;
import mrtjp.projectred.multipart.wiring.wires.EnumWire.WireDamageValues;
import mrtjp.projectred.renderstuffs.RenderIDs;
import mrtjp.projectred.renderstuffs.WireRenderAssistant;
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
import codechicken.core.render.CCRenderState;
import cpw.mods.fml.client.registry.ISimpleBlockRenderingHandler;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public class WireRenderer implements ISimpleBlockRenderingHandler {

	private RotatedRenderer rt = new RotatedRenderer();

	public final static WireRenderer instance = new WireRenderer();
	private WireRenderAssistant wra = new WireRenderAssistant();

	private final static boolean FAT_JACKETED_WIRE = false;

	private final static float SHADE_PY = 1.0f;
	private final static float SHADE_Z = 0.8f;
	private final static float SHADE_X = 0.6f;
	private final static float SHADE_NY = 0.5f;

	private int baseColour;

	public void renderWorld(RenderBlocks render, EnumWire type, TileWire wt, int sideMask, boolean renderJacketed) {
		wra = new WireRenderAssistant();
		wra.x = wt.xCoord;
		wra.y = wt.yCoord;
		wra.z = wt.zCoord;
		wra.renderBlocks = render;
		wra.wireIcon = (wt.getSpecialIconForRender() == null ? type.wireSprites[0] : wt.getSpecialIconForRender());
		wra.wireMap = type.wireMap;
		CCRenderState.reset();
		CCRenderState.setBrightness(wt.worldObj, wt.xCoord, wt.yCoord, wt.zCoord);
		CCRenderState.setColourOpaque(wt.getVisualWireColour());
		for (int i = 0; i < 6; i++) {
			if ((sideMask & (1 << i)) == 0)
				continue;
			wra.side = i;
			wra.setWireRenderState(wt);
			wra.pushRender();
		}
		return;
	}

	@Override
	public boolean renderWorldBlock(IBlockAccess world, int x, int y, int z, Block block, int modelId, RenderBlocks render) {
		TileWire wt = (TileWire) world.getBlockTileEntity(x, y, z);
		EnumWire type = wt.getType();
		if (type == null)
			return false;

		byte sideMask = wt.getSideMask();

		renderWorld(render, type, wt, sideMask, wt.hasJacketedWire());

		return true;
	}

	@Override
	public void renderInventoryBlock(Block block, int damageValue, int modelID, RenderBlocks render) {
		EnumWire type = WireDamageValues.getType(damageValue);
		if (type == null) {
			return;
		}

	}

	private void renderWireJacketed(RenderBlocks render, EnumWire type, int sideMask, boolean nx, boolean px, boolean ny, boolean py, boolean nz, boolean pz) {

	}

	public static boolean OLD_CORNER_SIDES = false;

	public static void renderWireSide(RotatedRenderer rt, RenderBlocks render, EnumWire type, boolean nz, boolean pz, boolean nx, boolean px, boolean nzCorner, boolean pzCorner, boolean nxCorner, boolean pxCorner, EnumWire nzCornerType, EnumWire pzCornerType, EnumWire nxCornerType, EnumWire pxCornerType, boolean forceEndCaps, boolean haveJacketed) {

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
