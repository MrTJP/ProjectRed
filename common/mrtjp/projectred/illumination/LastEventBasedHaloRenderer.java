package mrtjp.projectred.illumination;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;

import mrtjp.projectred.core.BasicRenderUtils;
import mrtjp.projectred.core.PRColors;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.entity.EntityLivingBase;
import net.minecraftforge.client.event.RenderWorldLastEvent;
import net.minecraftforge.event.ForgeSubscribe;

import org.bouncycastle.util.Arrays;
import org.lwjgl.opengl.GL11;

import codechicken.lib.render.CCRenderState;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public class LastEventBasedHaloRenderer {
	public static LastEventBasedHaloRenderer instance = new LastEventBasedHaloRenderer();

	private static Set<HaloObject> renderQueue = new HashSet<HaloObject>();

	public abstract static class HaloObject {
		int posX;
		int posY;
		int posZ;
		
		public HaloObject(int x, int y, int z) {
			posX = x;
			posY = y;
			posZ = z;
		}
		
		@Override
		public boolean equals(Object o) {
			if (o instanceof HaloObject) {
				HaloObject r = (HaloObject) o;
				return posX == r.posX && posY == r.posY && posZ == r.posZ;
			}
			return false;
		}
		public abstract void preRender();
		public abstract boolean render(RenderWorldLastEvent event);
		public abstract void postRender();
	}
	
	public static void addObjectToRender(HaloObject r) {
		for (HaloObject ro : renderQueue) {
			if (ro.equals(r)) {
				return;
			}
		}
		renderQueue.add(r);
	}

	@ForgeSubscribe
	public void onRenderWorldLast(RenderWorldLastEvent event) {
		List<HaloObject> removeQueue = new ArrayList<HaloObject>();
		GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE);
		GL11.glDisable(GL11.GL_TEXTURE_2D);
		GL11.glEnable(GL11.GL_BLEND);
		GL11.glPushMatrix();
		GL11.glDepthMask(false);
		EntityLivingBase view = Minecraft.getMinecraft().renderViewEntity;
		if (view != null) {
			double partials = event.partialTicks;
			double x = view.prevPosX + (view.posX - view.prevPosX) * partials;
			double y = view.prevPosY + (view.posY - view.prevPosY) * partials;
			double z = view.prevPosZ + (view.posZ - view.prevPosZ) * partials;
			GL11.glTranslated(-1 * x, -1 * y, -1 * z);
		}
		
		CCRenderState.reset();
		CCRenderState.startDrawing(7);
		for (HaloObject r : renderQueue) {
			r.preRender();
			if (!r.render(event)) {
				removeQueue.add(r);
			}
			r.postRender();
		}
		CCRenderState.draw();
		
		GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA);
		GL11.glEnable(GL11.GL_TEXTURE_2D);
		GL11.glColor3f(1, 1, 1);
		GL11.glDisable(GL11.GL_BLEND);
		GL11.glPopMatrix();
		GL11.glDepthMask(true);

		renderQueue.removeAll(removeQueue);
	}
}
