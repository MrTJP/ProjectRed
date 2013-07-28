package mrtjp.projectred.integration;

import mrtjp.projectred.multipart.wiring.RotatedRenderer;
import mrtjp.projectred.multipart.wiring.gates.BlockGate;
import mrtjp.projectred.multipart.wiring.gates.GateRenderBridge;
import mrtjp.projectred.multipart.wiring.gates.GateRenderBridge.Default;
import mrtjp.projectred.renderstuffs.RenderIDs;
import mrtjp.projectred.utils.BasicRenderUtils;
import mrtjp.projectred.utils.codechicken.core.render.CCRenderState;
import net.minecraft.block.Block;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.texture.TextureMap;
import net.minecraft.item.ItemStack;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.client.IItemRenderer;
import cpw.mods.fml.client.registry.ISimpleBlockRenderingHandler;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public class GateStaticRenderer implements IItemRenderer {

	private GateRenderBridge defaultRendering = new GateRenderBridge.Default();
	private RotatedRenderer rotatedRenderer = new RotatedRenderer();
	public static final GateStaticRenderer instance = new GateStaticRenderer();

	public boolean renderWorldBlock(TileGate te, int x, int y, int z) {
		int side = te.getSide();
		int front = te.getFront();
		EnumGate type = te.getGateType();
		if (type == null) {
			return true;
		}

		GateRenderBridge rendering = type.getRenderBridge();
		rendering.set(te.getRenderState());
		rotatedRenderer.x = x;
		rotatedRenderer.y = y;
		rotatedRenderer.z = z;
		rotatedRenderer.side = side;
		rotatedRenderer.front = front;

		BasicRenderUtils.bindTerrainResource();
		CCRenderState.reset();
		CCRenderState.setColour(0);
		Tessellator.instance.setColorRGBA(255, 255, 255, 255);
		CCRenderState.setBrightness(te.world(), te.x(), te.y(), te.z());
		rotatedRenderer.renderPartModel(rendering._modelBase, "base", .5f, 0, .5f, -1, -1, false);
		for (int i = 0; i < rendering.wireColor.length; i++) {
			float[] xPositions = rendering.wirePosX[i];
			float[] zPositions = rendering.wirePosZ[i];
			int color = rendering.wireColor[i];
			GateDynamicRenderer.renderWireOnGate(rotatedRenderer, xPositions, zPositions, rendering._wire, color);
		}
		for (int i = 0; i < rendering.torchState.length; i++) {
			GateDynamicRenderer.renderTorchOnGate(rotatedRenderer, rendering.torchX[i], rendering.torchY[i], rendering.torchZ[i], rendering.torchState[i], rendering._torchOn, rendering._torchOff);
		}
		for (int i = 0; i < rendering.pointerX.length; i++) {
			float xOffset = rendering.pointerX[i];
			float zOffset = rendering.pointerZ[i];
			GateDynamicRenderer.renderTorchOnGate(rotatedRenderer, rendering.pointerX[i], 0, rendering.pointerZ[i], true, rendering._torchOn, rendering._torchOff);
		}
		for (int i = 0; i < rendering.torchState.length; i++) {
			if (rendering.torchState[i]) {
				GateDynamicRenderer.renderGlowOnTorch(rotatedRenderer, rendering.torchX[i], rendering.torchY[i], rendering.torchZ[i], rendering._torchOn);
			}
		}
		for (int i = 0; i < rendering.pointerX.length; i++) {
			GateDynamicRenderer.renderGlowOnTorch(rotatedRenderer, rendering.pointerX[i], 0, rendering.pointerZ[i], rendering._torchOn);
		}
		rendering.renderSpecials(rotatedRenderer, false);
		return true;
	}

	public void renderInventoryBlock(int meta, int model, RenderBlocks render) {
		EnumGate type = EnumGate.VALUES[meta];
		GateRenderBridge rendering = (type == null ? defaultRendering : type.getRenderBridge());
		rendering.setItemRender();
		rotatedRenderer.x = 0;
		rotatedRenderer.y = 0;
		rotatedRenderer.z = 0;
		rotatedRenderer.front = 2;
		rotatedRenderer.side = 0;
		rotatedRenderer.renderBlocks = render;

		CCRenderState.reset();
		CCRenderState.useNormals(true);

		CCRenderState.startDrawing(7);
		rotatedRenderer.renderPartModel(rendering._modelBase, "base", .5f, 0, .5f, -1, -1, false);
		for (int i = 0; i < rendering.wireColor.length; i++) {
			float[] xPositions = rendering.wirePosX[i];
			float[] zPositions = rendering.wirePosZ[i];
			int color = rendering.wireColor[i];
			GateDynamicRenderer.renderWireOnGate(rotatedRenderer, xPositions, zPositions, rendering._wire, color);
		}

		for (int i = 0; i < rendering.torchState.length; i++) {
			GateDynamicRenderer.renderTorchOnGate(rotatedRenderer, rendering.torchX[i], rendering.torchY[i], rendering.torchZ[i], rendering.torchState[i], rendering._torchOn, rendering._torchOff);
		}

		for (int i = 0; i < rendering.pointerX.length; i++) {
			GateDynamicRenderer.renderPointerOnGateWithRotation(rotatedRenderer, rendering.pointerX[i], 0, rendering.pointerZ[i], rendering._pointer, 0);
			GateDynamicRenderer.renderTorchOnGate(rotatedRenderer, rendering.pointerX[i], 0, rendering.pointerZ[i], true, rendering._torchOn, rendering._torchOff);
		}
		for (int i = 0; i < rendering.torchState.length; i++) {
			if (rendering.torchState[i]) {
				GateDynamicRenderer.renderGlowOnTorch(rotatedRenderer, rendering.torchX[i], rendering.torchY[i], rendering.torchZ[i], rendering._torchOn);
			}
		}
		for (int i = 0; i < rendering.pointerX.length; i++) {
			GateDynamicRenderer.renderGlowOnTorch(rotatedRenderer, rendering.pointerX[i], 0, rendering.pointerZ[i], rendering._torchOn);
		}
		rendering.renderSpecials(rotatedRenderer, false);
		CCRenderState.draw();
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
		this.renderInventoryBlock(item.getItemDamage(), 0, null);
	}

}
