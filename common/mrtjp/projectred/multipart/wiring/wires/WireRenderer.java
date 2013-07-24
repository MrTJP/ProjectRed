package mrtjp.projectred.multipart.wiring.wires;

import org.lwjgl.opengl.GL11;

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

	public void renderWorld(RenderBlocks render, EnumWire type, TileWire wt, int sideMask, boolean renderJacketed) {
		wra = new WireRenderAssistant();
		wra.x = wt.xCoord;
		wra.y = wt.yCoord;
		wra.z = wt.zCoord;
		wra.renderBlocks = render;
		wra.wireIcon = (wt.getSpecialIconForRender() == null ? type.wireSprites[0] : wt.getSpecialIconForRender());
		CCRenderState.reset();
		CCRenderState.setBrightness(wt.worldObj, wt.xCoord, wt.yCoord, wt.zCoord);
		
		if (wt.hasJacketedWire()) {
			wra.model = type.jacketMap;
			wra.setJacketRender(wt);
			wra.pushJacketFrameRender();
			CCRenderState.setColourOpaque(wt.getVisualWireColour());
			wra.pushJacketWireRender();
		}
		wra.model = type.wireMap;
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
		wra = new WireRenderAssistant();
		wra.x = wra.y = wra.z = 0;
		wra.renderBlocks = render;
		wra.side = 0;
		wra.wireIcon = type.wireSprites[0];
		CCRenderState.reset();
		CCRenderState.useNormals(true);

		if (!WireDamageValues.isJacketed(damageValue)) {
			wra.model = type.wireMap;
			GL11.glPushMatrix();
			GL11.glColor4f(1, 1, 1, 1);
			GL11.glEnable(GL11.GL_LIGHTING);
			CCRenderState.setColourOpaque(type.itemColour);
			CCRenderState.startDrawing(7);
			wra.connectsN = true;
			wra.connectsS = true;
			wra.connectsW = true;
			wra.connectsE = true;
			wra.connectsInsideConnectorN = true;
			wra.connectsInsideConnectorS = true;
			wra.connectsInsideConnectorW = true;
			wra.connectsInsideConnectorE = true;
			wra.isCenterCrossed = true;
			wra.pushRender();
			CCRenderState.draw();
			GL11.glPopMatrix();
		} else if (WireDamageValues.isJacketed(damageValue)){
			wra.model = type.jacketMap;
			wra.setInventoryJacketRender();
			CCRenderState.startDrawing(7);
			wra.pushJacketFrameRender();
			CCRenderState.setColourOpaque(type.itemColour);
			wra.pushJacketWireRender();
			CCRenderState.draw();
		}
	}

	private void renderWireJacketed(RenderBlocks render, EnumWire type, int sideMask, boolean nx, boolean px, boolean ny, boolean py, boolean nz, boolean pz) {

	}

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
