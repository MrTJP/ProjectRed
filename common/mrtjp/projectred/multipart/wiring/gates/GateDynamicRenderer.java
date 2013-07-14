package mrtjp.projectred.multipart.wiring.gates;

import org.lwjgl.opengl.GL11;

import mrtjp.projectred.multipart.wiring.RotatedTessellator;
import mrtjp.projectred.utils.Color;
import net.minecraft.block.Block;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.tileentity.TileEntitySpecialRenderer;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.Icon;
import net.minecraftforge.client.IItemRenderer;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public class GateDynamicRenderer extends TileEntitySpecialRenderer implements IItemRenderer {

	public final static GateDynamicRenderer instance = new GateDynamicRenderer();
	RotatedTessellator rt = new RotatedTessellator();
	private GateRendering defaultRendering = new GateRendering.Default();

	@Override
	public void renderTileEntityAt(TileEntity var1, double x, double y, double z, float partialTick) {
		TileGate te = (TileGate) var1;
		if (te.getType() == null) {
			return;
		}

		EnumGate type = te.getType();
		GateRendering rendering = (type == null ? defaultRendering : type.getRendering());
		rendering.set(te.getRenderState());
		rendering._modelBase.renderPart("base", (float) x, (float) y, (float) z, te.getSide(), te.getFront(), .5f, 0, .5f);

		for (int i = 0; i < rendering.torchState.length; i++) {
			float xOffset = (16f - rendering.torchX[i]) / 16f;
			float yOffset = rendering.torchY[i] / 16f;
			float zOffset = (16f - rendering.torchZ[i]) / 16f;
			boolean on = rendering.torchState[i];
			renderTorchOnGate((float) x, (float) y, (float) z, te.getSide(), te.getFront(), xOffset, yOffset, zOffset, on, rendering);
		}

		for (int k = 0; k < rendering.pointerX.length; k++) {
			float xOffset = rendering.pointerX[k] / 16f;
			float zOffset = rendering.pointerZ[k] / 16f;
			renderPointer(xOffset, 0, zOffset, 0);
		}

		if (true)
			return;
		GateRendering gr = te.getType().getRendering();

		// System.out.println(te.getType()+", "+gr.pointerX.length+" pointers");

		if (gr.pointerX.length == 0) {
			return;
		}

		Minecraft.getMinecraft().renderEngine.bindTexture("/terrain.png");

		rt.base = Tessellator.instance;
		rt.x = x;
		rt.y = y;
		rt.z = z;
		rt.front = te.getFront();
		rt.side = te.getSide();
		rt.flipped = te.isFlipped();

		rt.base.startDrawingQuads();
		for (int k = 0; k < gr.pointerX.length; k++) {
			renderPointer(gr.pointerX[k] / 16.0f, gr.pointerZ[k] / 16.0f, -(te.pointerPos + te.pointerSpeed * partialTick) * Math.PI / 180, 1 / 16f);
		}
		rt.base.draw();
	}

	private void renderPointer(float x, float z, double angle, float pixel) {
		if (true)
			return;
		rt.base = Tessellator.instance;
		rt.x = 0;
		rt.y = 0;
		rt.z = 0;
		rt.front = 3;
		rt.side = 0;
		rt.flipped = false;
		rt.base.startDrawingQuads();

		// x' = x * cos(angle) + z * sin(angle)
		// z' = z * cos(angle) - x * sin(angle)

		Icon i = Block.stone.getIcon(0, 0);

		float sin = (float) Math.sin(angle);
		float cos = (float) Math.cos(angle);

		float x1 = -(3 * pixel) * cos;
		float z1 = +(3 * pixel) * sin;

		float x2 = -(6 * pixel) * sin;
		float z2 = -(6 * pixel) * cos;

		float x3 = +(3 * pixel) * cos;
		float z3 = -(3 * pixel) * sin;

		float x4 = +(3 * pixel) * sin;
		float z4 = +(3 * pixel) * cos;

		float y1 = 5 / 16f;
		float y2 = 7 / 16f;

		float side_u1 = i.getInterpolatedU(4);
		float side_u2 = i.getInterpolatedU(12);
		float side_v1 = i.getInterpolatedV(7);
		float side_v2 = i.getInterpolatedV(9);

		float u_centre = i.getInterpolatedU(8);
		float v_centre = i.getInterpolatedV(8);
		float u_min = i.getInterpolatedU(5);
		float u_max = i.getInterpolatedU(11);
		float v_min = i.getInterpolatedV(5);
		float v_max = i.getInterpolatedV(14);

		rt.addVertexWithUV(x + x1, y1, z + z1, side_u1, side_v1);
		rt.addVertexWithUV(x + x1, y2, z + z1, side_u1, side_v2);
		rt.addVertexWithUV(x + x2, y2, z + z2, side_u2, side_v2);
		rt.addVertexWithUV(x + x2, y1, z + z2, side_u2, side_v1);

		rt.addVertexWithUV(x + x3, y2, z + z3, side_u1, side_v1);
		rt.addVertexWithUV(x + x3, y1, z + z3, side_u1, side_v2);
		rt.addVertexWithUV(x + x2, y1, z + z2, side_u2, side_v2);
		rt.addVertexWithUV(x + x2, y2, z + z2, side_u2, side_v1);

		rt.addVertexWithUV(x + x3, y1, z + z3, side_u1, side_v1);
		rt.addVertexWithUV(x + x3, y2, z + z3, side_u1, side_v2);
		rt.addVertexWithUV(x + x4, y2, z + z4, side_u2, side_v2);
		rt.addVertexWithUV(x + x4, y1, z + z4, side_u2, side_v1);

		rt.addVertexWithUV(x + x1, y2, z + z1, side_u1, side_v1);
		rt.addVertexWithUV(x + x1, y1, z + z1, side_u1, side_v2);
		rt.addVertexWithUV(x + x4, y1, z + z4, side_u2, side_v2);
		rt.addVertexWithUV(x + x4, y2, z + z4, side_u2, side_v1);

		rt.addVertexWithUV(x + x1, y1, z + z1, u_min, v_centre);
		rt.addVertexWithUV(x + x2, y1, z + z2, u_centre, v_max);
		rt.addVertexWithUV(x + x3, y1, z + z3, u_max, v_centre);
		rt.addVertexWithUV(x + x4, y1, z + z4, u_centre, v_min);

		rt.addVertexWithUV(x + x4, y2, z + z4, u_centre, v_min);
		rt.addVertexWithUV(x + x3, y2, z + z3, u_max, v_centre);
		rt.addVertexWithUV(x + x2, y2, z + z2, u_centre, v_max);
		rt.addVertexWithUV(x + x1, y2, z + z1, u_min, v_centre);
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
		switch (type) {
		case ENTITY:
			renderInventoryBlock(item.getItemDamage(), 0f, 0f, 0f, 1f);
			return;
		case EQUIPPED:
			renderInventoryBlock(item.getItemDamage(), 0f, .5f, 0f, 1f);
			return;
		case EQUIPPED_FIRST_PERSON:
			renderInventoryBlock(item.getItemDamage(), 0f, .5f, 0f, 1f);
			return;
		case INVENTORY:
			renderInventoryBlock(item.getItemDamage(), 0f, 0f, 0f, 1f);
			return;
		default:
			return;
		}

	}

	public void renderInventoryBlock(int meta, float x, float y, float z, float scale) {
		GL11.glPushMatrix();
		GL11.glScalef(scale, scale, scale);
		EnumGate type = EnumGate.VALUES[meta];
		GateRendering rendering = (type == null ? defaultRendering : type.getRendering());
		rendering.setItemRender();
		rendering._modelBase.renderPart("base", x, y, z, 0, 3, .5f, 0, .5f);

		for (int i = 0; i < rendering.torchState.length; i++) {
			float xOffset = (16f - rendering.torchX[i]) / 16f;
			float yOffset = rendering.torchY[i] / 16f;
			float zOffset = (16f - rendering.torchZ[i]) / 16f;
			boolean on = rendering.torchState[i];
			renderTorchOnGate((float) x, (float) y, (float) z, 0, 2, xOffset, yOffset, zOffset, on, rendering);
		}

		for (int k = 0; k < rendering.pointerX.length; k++) {
			float xOffset = rendering.pointerX[k] / 16f;
			float zOffset = rendering.pointerZ[k] / 16f;
			renderPointer(xOffset, 0, zOffset, 0);
		}
		GL11.glPopMatrix();
	}

	/**
	 * Render the torch on a gate. x,y,z are the gate coords. side and facing
	 * are for rotation and attached side. xOffset, yOffset, and zOffset are
	 * where on the actual gate they are. Note that 0.3f are subtracted from
	 * xOffset and added to zOffset to account for torch thickness.
	 * 
	 * @param x
	 * @param y
	 * @param z
	 * @param side
	 * @param facing
	 * @param xOffset
	 * @param yOffset
	 * @param zOffset
	 * @param on
	 * @param rendering
	 */
	public void renderTorchOnGate(float x, float y, float z, int side, int facing, float xOffset, float yOffset, float zOffset, boolean on, GateRendering rendering) {
		if (!on) {
			GL11.glPushMatrix();
			GL11.glDisable(GL11.GL_LIGHTING);
			GL11.glColor4f(1, 1, 1, 1);
			rendering._torchOff.renderPart("torch", x, y, z, side, facing, xOffset + .03f, yOffset, zOffset - .03f);
			GL11.glEnable(GL11.GL_LIGHTING);
			GL11.glPopMatrix();
			return;
		} else {
			GL11.glPushMatrix();
			GL11.glDisable(GL11.GL_LIGHTING);
			rendering._torchOn.renderPart("torch", x, y, z, side, facing, xOffset + .03f, yOffset, zOffset - .03f);
			GL11.glDisable(GL11.GL_TEXTURE_2D);
			GL11.glEnable(GL11.GL_BLEND);
			GL11.glColor4f(Color.RED.r, Color.RED.g, Color.RED.b, .5f);
			GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE);
			GL11.glDepthMask(false);
			rendering._torchOn.renderPart("glow", x, y, z, side, facing, xOffset + .03f, yOffset, zOffset - .03f);
			GL11.glEnable(GL11.GL_TEXTURE_2D);
			GL11.glDisable(GL11.GL_BLEND);
			GL11.glColor4f(1, 1, 1, 1);
			GL11.glEnable(GL11.GL_LIGHTING);
			GL11.glDepthMask(true);
			GL11.glPopMatrix();
			return;
		}
	}

}
