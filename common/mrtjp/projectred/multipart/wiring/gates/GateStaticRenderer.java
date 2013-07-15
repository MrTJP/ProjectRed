package mrtjp.projectred.multipart.wiring.gates;

import static org.lwjgl.opengl.GL11.GL_BLEND;
import static org.lwjgl.opengl.GL11.GL_GREATER;
import static org.lwjgl.opengl.GL11.GL_LIGHTING;
import static org.lwjgl.opengl.GL11.GL_ONE_MINUS_SRC_ALPHA;
import static org.lwjgl.opengl.GL11.GL_SRC_ALPHA;
import static org.lwjgl.opengl.GL11.glAlphaFunc;
import static org.lwjgl.opengl.GL11.glBlendFunc;
import static org.lwjgl.opengl.GL11.glDisable;
import static org.lwjgl.opengl.GL11.glEnable;
import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.multipart.wiring.RotatedTessellator;
import mrtjp.projectred.renderstuffs.RenderIDs;
import mrtjp.projectred.renderstuffs.gates.GatePartModel;
import mrtjp.projectred.utils.Color;
import mrtjp.projectred.utils.Dir;
import net.minecraft.block.Block;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.util.Icon;
import net.minecraft.world.IBlockAccess;

import org.lwjgl.opengl.GL11;

import cpw.mods.fml.client.registry.ISimpleBlockRenderingHandler;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@Deprecated
@SideOnly(Side.CLIENT)
public class GateStaticRenderer implements ISimpleBlockRenderingHandler {

	private GateRenderBridge defaultRendering = new GateRenderBridge.Default();
	private RotatedTessellator rotatedTessellator = new RotatedTessellator();
	public static final GateStaticRenderer instance = new GateStaticRenderer();

	@Override
	public boolean renderWorldBlock(IBlockAccess world, int x, int y, int z, Block block, int model, RenderBlocks render) {
		TileGate te = (TileGate) world.getBlockTileEntity(x, y, z);
		int side = te.getSide();
		int front = te.getFront();
		EnumGate type = te.getType();
		if (type == null) {
			return true;
		}
		// We need a new instance of the Tessellator to draw.
		GateRenderBridge rendering = type.getRendering();
		rendering.set(te.getRenderState());
		//rendering._modelBase.renderPart("base", x, y, z, te.getSide(), te.getFront(), 0, 0, 0);
		
		//renderGateSurface(block, side, front, rendering.segmentIcons[0]);
		if (true) return true;
		for (int k = 0; k < rendering.segmentTex.length; k++) {
			Tessellator.instance.setColorOpaque_I(rendering.wireColor[k]);
		}

		rotatedTessellator.base = Tessellator.instance;
		rotatedTessellator.front = front;
		rotatedTessellator.side = side;
		rotatedTessellator.x = x;
		rotatedTessellator.y = y;
		rotatedTessellator.z = z;
		rotatedTessellator.flipped = te.isFlipped();
		Tessellator.instance.setBrightness(world.getLightBrightnessForSkyBlocks(x, y, z, 0));
		Minecraft.getMinecraft().renderEngine.bindTexture("/terrain.png");
		
		Tessellator.instance.setBrightness(world.getLightBrightnessForSkyBlocks(x, y, z, 0));
		Tessellator.instance.setColorRGBA(255, 255, 255, 255);
		rotatedTessellator.base = Tessellator.instance;
		for (int k = 0; k < rendering.torchState.length; k++) {
			float tx = rendering.torchX[k] / 16f;
			float ty = rendering.torchZ[k] / 16f;
			boolean on = rendering.torchState[k];
			renderTorchAtAngle(render, on ? rendering.torchTexOn : rendering.torchTexOff, tx, ty, 3 / 16f);
		}

		for (int k = 0; k < rendering.pointerX.length; k++) {
			float tx = rendering.pointerX[k] / 16f;
			float ty = rendering.pointerZ[k] / 16f;
			renderTorchAtAngle(render, rendering.torchTexOn, tx, ty, 0f);
		}
		rendering.customRender(rotatedTessellator, render);

		if (true)
			return true;

		if (side < 0 || side > 5 || front < 0 || front > 5) {
			// invalid orientation, make it obvious
			side = front = 0;
		}
		switch (side) {
		case Dir.NX:
			render.setRenderBounds(0, 0, 0, BlockGate.THICKNESS, 1, 1);
			break;
		case Dir.NY:
			render.setRenderBounds(0, 0, 0, 1, BlockGate.THICKNESS, 1);
			break;
		case Dir.NZ:
			render.setRenderBounds(0, 0, 0, 1, 1, BlockGate.THICKNESS);
			break;
		case Dir.PX:
			render.setRenderBounds(1 - BlockGate.THICKNESS, 0, 0, 1, 1, 1);
			break;
		case Dir.PY:
			render.setRenderBounds(0, 1 - BlockGate.THICKNESS, 0, 1, 1, 1);
			break;
		case Dir.PZ:
			render.setRenderBounds(0, 0, 1 - BlockGate.THICKNESS, 1, 1, 1);
			break;
		}
		if ((side & 6) == (front & 6)) {
			// invalid orientation, make it obvious
			render.setRenderBounds(0, 0, 0, 1, 1, 1);
		}

		// System.out.println(side+" "+front+" -> "+rotationLookup[side][front]);

		BlockGate.renderSide = -(side ^ 1) - 1;
		BlockGate.textureOverride = null;
		BlockGate.colourOverride = -1;
		render.renderStandardBlock(block, x, y, z);
		return true;
	}

	@Override
	public void renderInventoryBlock(Block block, int meta, int model, RenderBlocks render) {

		EnumGate type = EnumGate.VALUES[meta];
		GateRenderBridge rendering = (type == null ? defaultRendering : type.getRendering());
		rendering.setItemRender();
		rendering._modelBase.renderPart("base", 0, -.5f, 0, 0, 3, 0, 0, 0);
		
		for (int i = 0; i < rendering.torchState.length; i++) {
			float xOffset = rendering.torchX[i] / 16f;
			//float yOffset = rendering.torchY[i] / 16f;
			float yOffset = 0;
			float zOffset = rendering.torchZ[i] / 16f;
			boolean on = rendering.torchState[i];
			renderTorchOnGate(0, 0, 0, 0, 3, xOffset - .5f, yOffset -.5f, zOffset - .5f, on, rendering);
		}
		
		

		
		if (true) return;
		render.setRenderBounds(0, 0, 0, 1, BlockGate.THICKNESS, 1);

		Minecraft.getMinecraft().renderEngine.bindTexture("/terrain.png");

		Tessellator t = Tessellator.instance;

		BlockGate.textureOverride = null;
		BlockGate.renderTypeOverride = 0;

		glAlphaFunc(GL_GREATER, 0);
		glEnable(GL_BLEND);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

		rotatedTessellator.base = Tessellator.instance;
		rotatedTessellator.front = Dir.NZ;
		rotatedTessellator.side = Dir.NY;
		rotatedTessellator.x = -0.5;
		rotatedTessellator.y = -0.2;
		rotatedTessellator.z = -0.5;
		rotatedTessellator.flipped = false;

		GL11.glColor3f(1, 1, 1);
		t.startDrawingQuads();
		t.setBrightness(0x00F000F0);
		t.setColorRGBA(255, 255, 255, 255);
		t.setColorOpaque_I(0xFFFFFF);
		Icon tex = ProjectRed.blockGate.getIcon(0, 0);
		t.setNormal(-1.0F, 0.0F, 0.0F);
		render.renderFaceXNeg(ProjectRed.blockGate, -0.5, rotatedTessellator.y, -0.5, tex);
		t.setNormal(1.0F, 0.0F, 0.0F);
		render.renderFaceXPos(ProjectRed.blockGate, -0.5, rotatedTessellator.y, -0.5, tex);
		t.setNormal(0.0F, 0.0F, -1.0F);
		render.renderFaceZNeg(ProjectRed.blockGate, -0.5, rotatedTessellator.y, -0.5, tex);
		t.setNormal(0.0F, 0.0F, 1.0F);
		render.renderFaceZPos(ProjectRed.blockGate, -0.5, rotatedTessellator.y, -0.5, tex);
		t.setNormal(0.0F, -1.0F, 0.0F);
		render.renderFaceYNeg(ProjectRed.blockGate, -0.5, rotatedTessellator.y, -0.5, tex);
		t.setNormal(0.0F, 1.0F, 0.0F);

		t.setBrightness(0x00F000F0);
		t.setColorRGBA(255, 255, 255, 255);

		// Render the block surface.
		for (int k = 0; k < rendering.segmentIcons.length; k++) {
			t.setColorOpaque_I(rendering.wireColor[k]);
			renderGateSurface(ProjectRed.blockGate, rotatedTessellator.side, rotatedTessellator.front, rendering.segmentIcons[k]);
		}

		Tessellator.instance.setColorRGBA(255, 255, 255, 255);
		rendering.customRender(rotatedTessellator, render);
		t.draw();

		glDisable(GL_BLEND);
		glEnable(GL_LIGHTING);

		BlockGate.renderTypeOverride = -1;

		glDisable(GL_LIGHTING);
		Tessellator.instance.startDrawingQuads();
		Tessellator.instance.setBrightness(0x00F000F0);
		Tessellator.instance.setColorRGBA(255, 255, 255, 255);


		for (int k = 0; k < rendering.pointerX.length; k++) {
			float tx = rendering.pointerX[k] / 16f;
			float ty = rendering.pointerZ[k] / 16f;

			renderTorchAtAngle(render, rendering.torchTexOn, tx, ty, 0f);
		}

		Tessellator.instance.draw();

		render.uvRotateTop = 0;
		glEnable(GL_LIGHTING);
		glDisable(GL_BLEND);
	}

	public void renderTorchOnGate(int x, int y, int z, int side, int facing, float xOffset, float yOffset, float zOffset, boolean on, GateRenderBridge rendering) {
		if (on) {
			rendering._torchOn.renderPart("torch", x, y, z, side, facing, xOffset, yOffset, zOffset);
			GL11.glPushMatrix();
			GL11.glDisable(GL11.GL_TEXTURE_2D);
			GL11.glEnable(GL11.GL_BLEND);
			GL11.glColor4f(Color.RED.r, Color.RED.g, Color.RED.b, .5f);
			GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE);
			GL11.glDisable(GL11.GL_LIGHTING);
			GL11.glDepthMask(false);
			rendering._torchOn.renderPart("glow", x, y, z, side, facing, xOffset, yOffset, zOffset);
			GL11.glEnable(GL11.GL_TEXTURE_2D);
			GL11.glDisable(GL11.GL_BLEND);
			GL11.glEnable(GL11.GL_LIGHTING);
			GL11.glDepthMask(true);
			GL11.glPopMatrix();
		} else {
			rendering._torchOff.renderPart("torch", x, y, z, side, facing, xOffset, yOffset, zOffset);
		}
	}
	
	
	public void renderTorchAtAngle(RenderBlocks render, Icon texture, double x, double z, float Y_INSET) {
		RotatedTessellator var12 = rotatedTessellator;

		if (render.overrideBlockTexture != null) {
			texture = render.overrideBlockTexture;
		}

		float var16 = texture.getMinU();
		float var17 = texture.getMaxU();
		float var18 = texture.getMinV();
		float var19 = texture.getInterpolatedV(16 - Y_INSET * 16);
		double var20 = texture.getInterpolatedU(7);
		double var22 = texture.getInterpolatedV(6);
		double var24 = texture.getInterpolatedU(9);
		double var26 = texture.getInterpolatedV(8);
		double var44 = 0.0625D;
		double var46 = 0.625D - Y_INSET;

		var12.addVertexWithUV(x - var44, var46, z - var44, var20, var22);
		var12.addVertexWithUV(x - var44, var46, z + var44, var20, var26);
		var12.addVertexWithUV(x + var44, var46, z + var44, var24, var26);
		var12.addVertexWithUV(x + var44, var46, z - var44, var24, var22);

		var12.addVertexWithUV(x - var44, 1 - Y_INSET, z - 0.5, (double) var16, (double) var18);
		var12.addVertexWithUV(x - var44, 0, z - 0.5, (double) var16, (double) var19);
		var12.addVertexWithUV(x - var44, 0, z + 0.5, (double) var17, (double) var19);
		var12.addVertexWithUV(x - var44, 1 - Y_INSET, z + 0.5, (double) var17, (double) var18);

		var12.addVertexWithUV(x + var44, 1 - Y_INSET, z + 0.5, (double) var16, (double) var18);
		var12.addVertexWithUV(x + var44, 0, z + 0.5, (double) var16, (double) var19);
		var12.addVertexWithUV(x + var44, 0, z - 0.5, (double) var17, (double) var19);
		var12.addVertexWithUV(x + var44, 1 - Y_INSET, z - 0.50, (double) var17, (double) var18);

		var12.addVertexWithUV(x - 0.5, 1 - Y_INSET, z + var44, (double) var16, (double) var18);
		var12.addVertexWithUV(x - 0.5, 0, z + var44, (double) var16, (double) var19);
		var12.addVertexWithUV(x + 0.5, 0, z + var44, (double) var17, (double) var19);
		var12.addVertexWithUV(x + 0.5, 1 - Y_INSET, z + var44, (double) var17, (double) var18);

		var12.addVertexWithUV(x + 0.5, 1 - Y_INSET, z - var44, (double) var16, (double) var18);
		var12.addVertexWithUV(x + 0.5, 0, z - var44, (double) var16, (double) var19);
		var12.addVertexWithUV(x - 0.5, 0, z - var44, (double) var17, (double) var19);
		var12.addVertexWithUV(x - 0.5, 1 - Y_INSET, z - var44, (double) var17, (double) var18);
	}

	private void renderGateSurface(Block block, int side, int front, Icon tex) {
		
		if (true) return;
		final double u1 = tex.getMinU();
		final double v1 = tex.getMinV();
		final double u2 = tex.getMaxU();
		final double v2 = v1;
		final double u3 = u2;
		final double v3 = tex.getMaxV();
		final double u4 = u1;
		final double v4 = v3;
		final int x = 0;
		final int z = 0;
		final double y = BlockGate.THICKNESS;

		rotatedTessellator.addVertexWithUV(x, y, z, u1, v1);
		rotatedTessellator.addVertexWithUV(x, y, z + 1, u4, v4);
		rotatedTessellator.addVertexWithUV(x + 1, y, z + 1, u3, v3);
		rotatedTessellator.addVertexWithUV(x + 1, y, z, u2, v2);

	}

	@Override
	public boolean shouldRender3DInInventory() {
		return true;
	}

	@Override
	public int getRenderId() {
		return RenderIDs.renderIdGate;
	}

}
