package mrtjp.projectred.multipart.wiring.wires;

import mrtjp.projectred.multipart.wiring.RotatedRenderer;
import mrtjp.projectred.multipart.wiring.wires.EnumWire.WireDamageValues;
import mrtjp.projectred.renderstuffs.RenderIDs;
import mrtjp.projectred.transmission.WirePart;
import mrtjp.projectred.transmission.WireRenderAssistant;
import mrtjp.projectred.utils.BasicRenderUtils;
import net.minecraft.block.Block;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.world.IBlockAccess;
import codechicken.lib.render.CCRenderState;
import cpw.mods.fml.client.registry.ISimpleBlockRenderingHandler;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public class WireRenderer implements ISimpleBlockRenderingHandler {

	private RotatedRenderer rt = new RotatedRenderer();

	public final static WireRenderer instance = new WireRenderer();
	private WireRenderAssistant wra = new WireRenderAssistant();

	public void renderWorld(RenderBlocks render, EnumWire type, WirePart wt, int sideMask, boolean renderJacketed) {
		wra = new WireRenderAssistant();
		wra.x = wt.xCoord;
		wra.y = wt.yCoord;
		wra.z = wt.zCoord;
		wra.renderBlocks = render;
		wra.wireIcon = (wt.getSpecialIconForRender() == null ? type.wireSprites[0] : wt.getSpecialIconForRender());
		Tessellator.instance.setColorRGBA(255, 255, 255, 255);
		BasicRenderUtils.bindTerrainResource();
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
		WirePart wt = (WirePart) world.getBlockTileEntity(x, y, z);
		EnumWire type = wt.getWireType();
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
		BasicRenderUtils.bindTerrainResource();
		CCRenderState.reset();
		CCRenderState.useNormals(true);

		if (!WireDamageValues.isJacketed(damageValue)) {
			wra.model = type.wireMap;
			CCRenderState.startDrawing(7);
			BasicRenderUtils.setFullColor();
			BasicRenderUtils.setFullBrightness();
			CCRenderState.setColourOpaque(type.itemColour);
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
		} else if (WireDamageValues.isJacketed(damageValue)){
			wra.model = type.jacketMap;
			wra.setInventoryJacketRender();
			CCRenderState.startDrawing(7);
			BasicRenderUtils.setFullColor();
			BasicRenderUtils.setFullBrightness();
			wra.pushJacketFrameRender();
			CCRenderState.setColourOpaque(type.itemColour);
			wra.pushJacketWireRender();
			CCRenderState.draw();
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
