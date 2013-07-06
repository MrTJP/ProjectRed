package mrtjp.projectred.multipart.wiring.gates;

import mrtjp.projectred.multipart.wiring.RotatedTessellator;
import net.minecraft.block.Block;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.tileentity.TileEntitySpecialRenderer;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.Icon;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public class GateDynamicRenderer extends TileEntitySpecialRenderer {

	public final static GateDynamicRenderer instance = new GateDynamicRenderer();
	RotatedTessellator rt = new RotatedTessellator();
	
	@Override
	public void renderTileEntityAt(TileEntity var1, double x, double y, double z, float partialTick) {
		TileGate te = (TileGate)var1;
		if(te.getType() == null)
			return;
		
		GateRendering gr = te.getType().getRendering();
		
		//System.out.println(te.getType()+", "+gr.pointerX.length+" pointers");
		
		if(gr.pointerX.length == 0)
			return;
		
		Minecraft.getMinecraft().renderEngine.bindTexture("/terrain.png");
		
		rt.base = Tessellator.instance;
		rt.x = x;
		rt.y = y;
		rt.z = z;
		rt.front = te.getFront();
		rt.side = te.getSide();
		rt.flipped = te.isFlipped();
		
		rt.base.startDrawingQuads();
		for(int k = 0; k < gr.pointerX.length; k++) {
			renderPointer(gr.pointerX[k]/16.0f, gr.pointerY[k]/16.0f, -(te.pointerPos + te.pointerSpeed * partialTick) * Math.PI / 180, 1/16f);
		}
		rt.base.draw();
	}
	
	private void renderPointer(float x, float z, double angle, float pixel) {
		// x' = x * cos(angle) + z * sin(angle)
		// z' = z * cos(angle) - x * sin(angle)
		
		Icon i = Block.stone.getIcon(0, 0);
		
		float sin = (float)Math.sin(angle);
		float cos = (float)Math.cos(angle);

		float x1 = -(3*pixel) * cos;
		float z1 = +(3*pixel) * sin;
		
		float x2 = -(6*pixel) * sin;
		float z2 = -(6*pixel) * cos;
		
		float x3 = +(3*pixel) * cos;
		float z3 = -(3*pixel) * sin;
		
		float x4 = +(3*pixel) * sin;
		float z4 = +(3*pixel) * cos;
		
		float y1 = 5/16f;
		float y2 = 7/16f;
		
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

}
