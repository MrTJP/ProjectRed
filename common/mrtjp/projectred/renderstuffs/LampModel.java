package mrtjp.projectred.renderstuffs;

import org.lwjgl.opengl.GL11;

import mrtjp.projectred.blocks.BlockLamp;
import mrtjp.projectred.blocks.BlockLamp.EnumLamp;
import mrtjp.projectred.tiles.TileLamp;
import mrtjp.projectred.utils.Color;
import net.minecraft.util.Icon;
import net.minecraftforge.client.model.AdvancedModelLoader;
import net.minecraftforge.client.model.IModelCustom;
import cpw.mods.fml.client.FMLClientHandler;

public class LampModel {

	private IModelCustom model;

	public LampModel() {
		model = AdvancedModelLoader.loadModel("/mods/projectred/textures/obj/lamp.obj");
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

		GL11.glColor4f(Color.get(tint).r, Color.get(tint).g, Color.get(tint).b, 0.6f);
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
