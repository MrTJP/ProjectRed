package mrtjp.projectred.renderstuffs;

import org.lwjgl.opengl.GL11;

import mrtjp.projectred.blocks.BlockLamp;
import mrtjp.projectred.blocks.BlockLamp.EnumLamp;
import mrtjp.projectred.blocks.BlockLantern.EnumLantern;
import mrtjp.projectred.tiles.TileLamp;
import mrtjp.projectred.utils.Color;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.RenderEngine;
import net.minecraft.util.Icon;
import net.minecraftforge.client.model.AdvancedModelLoader;
import net.minecraftforge.client.model.IModelCustom;
import cpw.mods.fml.client.FMLClientHandler;

public class LanternModel {

	private IModelCustom model;

	public LanternModel() {
		model = AdvancedModelLoader.loadModel("/mods/projectred/textures/obj/lantern.obj");
	}

	public void render() {
		model.renderAll();
	}

	public void renderPart(String part) {
		model.renderPart(part);
	}

	public void bindTextureForColorAndState(int color, boolean on) {
		String base = "/mods/projectred/textures/blocks/";
		String folder = on ? "lanternon/" : "lanternoff/";
		String file = EnumLantern.get(color).unlocalName + (on ? "on" : "off");
		String loc = base + folder + file + ".png";
		bindTextureByName(loc);
	}

	protected void bindTextureByName(String par1Str) {
		RenderEngine renderengine = Minecraft.getMinecraft().renderEngine;
		if (renderengine != null) {
			renderengine.bindTexture(par1Str);
		}
	}

	public void renderInventory(double x, double y, double z, float scale) {
		GL11.glPushMatrix();
		GL11.glTranslated(x, y, z);
		GL11.glScalef(scale, scale, scale);
		GL11.glRotatef(180, 0f, 1f, 0f);
		model.renderPart("lamp");
		model.renderPart("covertop");
		model.renderPart("coverbottom");
		model.renderPart("goldringtop");
		GL11.glPopMatrix();
	}

	public void renderLampAndCovers(double x, double y, double z) {

		// Render lantern bulb, and top and bottom covers.
		GL11.glPushMatrix();
		// Edit GL state
		// This will be rendered on basic state...

		GL11.glTranslated(x, y, z + 1);
		model.renderPart("lamp");
		model.renderPart("covertop");
		model.renderPart("coverbottom");

		GL11.glPopMatrix();

		// Undo GL state changes
		// Nothing to undo.
	}

	public void renderCenterStand(double x, double y, double z, int rotation) {
		// Render lantern holder at the top or bottom.
		GL11.glPushMatrix();
		GL11.glTranslated(x, y, z + 1);
		if (rotation == 1) {
			model.renderPart("goldringtop");
			model.renderPart("standtop");
		} else if (rotation == 0) {
			model.renderPart("goldringbottom");
			model.renderPart("standbottom");
		}
		GL11.glPopMatrix();
	}

	public void renderSideStand(double x, double y, double z, int rotation) {
		// Render lantern holder connected to sides..
		GL11.glPushMatrix();
		rotateByDirection(x, y, z, rotation);
		model.renderPart("goldringtop");
		model.renderPart("standside");
		GL11.glPopMatrix();
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
		model.renderPart("lampshade");
		GL11.glPopMatrix();

		// Undo GL state changes
		GL11.glEnable(GL11.GL_TEXTURE_2D);
		GL11.glDisable(GL11.GL_BLEND);
		GL11.glBlendFunc(GL11.GL_ONE_MINUS_SRC_ALPHA, GL11.GL_ONE);
		GL11.glEnable(GL11.GL_LIGHTING);
		GL11.glDepthMask(true);
	}

	private void rotateByDirection(double x, double y, double z, int direction) {

		switch (direction) {
		case 2: {
			GL11.glTranslatef((float) x + 0.0F, (float) y + 0.0F, (float) z + 0.0F);
			GL11.glRotatef(-90F, 0F, 1F, 0F);
			return;
		}
		case 3: {
			GL11.glTranslatef((float) x + 1.0F, (float) y + 0.0F, (float) z + 1.0F);
			GL11.glRotatef(90F, 0F, 1F, 0F);
			return;
		}
		case 4: {
			GL11.glTranslatef((float) x + 0.0F, (float) y + 0.0F, (float) z + 1.0F);
			GL11.glRotatef(0F, 0F, 1F, 0F);
			return;
		}
		case 5: {
			GL11.glTranslatef((float) x + 1.0F, (float) y + 0.0F, (float) z + 0.0F);
			GL11.glRotatef(180F, 0F, 1F, 0F);
			return;
		}
		default: {
			return;
		}
		}
	}

}
