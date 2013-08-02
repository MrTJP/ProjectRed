package mrtjp.projectred.renderstuffs;

import mrtjp.projectred.utils.PRColors;
import net.minecraftforge.client.model.AdvancedModelLoader;
import net.minecraftforge.client.model.IModelCustom;

import org.lwjgl.opengl.GL11;

public class LampModel {

	private IModelCustom model;

	public LampModel() {
		model = AdvancedModelLoader.loadModel("/assets/projectred/textures/obj/lamp.obj");
	}

	public void render() {
		model.renderAll();
	}

	public void renderPart(String part) {
		model.renderPart(part);
	}

	public void renderLampShade(double x, double y, double z, int tint) {
		// Render lamp shade
		GL11.glPushMatrix();
		// Edit GL state
		GL11.glDisable(GL11.GL_TEXTURE_2D);
		GL11.glEnable(GL11.GL_BLEND);
		GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE);
		GL11.glDisable(GL11.GL_LIGHTING);
		GL11.glDepthMask(false);

		GL11.glColor4f(PRColors.get(tint).r, PRColors.get(tint).g, PRColors.get(tint).b, 0.6f);
		GL11.glTranslated(x, y, z + 1);
		model.renderPart("shade");
		GL11.glPopMatrix();

		// Undo GL state changes
		GL11.glEnable(GL11.GL_TEXTURE_2D);
		GL11.glDisable(GL11.GL_BLEND);
		GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE);
		GL11.glEnable(GL11.GL_LIGHTING);
		GL11.glDepthMask(true);
	}
}
