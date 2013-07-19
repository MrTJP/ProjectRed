package mrtjp.projectred.renderstuffs;

import java.util.Map;

import mrtjp.projectred.blocks.BlockLantern.EnumLantern;
import mrtjp.projectred.utils.Color;
import mrtjp.projectred.utils.codechicken.core.render.CCModel;
import mrtjp.projectred.utils.codechicken.core.render.CCRenderState;
import mrtjp.projectred.utils.codechicken.core.vec.InvertX;
import mrtjp.projectred.utils.codechicken.core.vec.Rotation;
import mrtjp.projectred.utils.codechicken.core.vec.Translation;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.RenderEngine;

import org.lwjgl.opengl.GL11;


public class LanternModel {

	private Map<String, CCModel> models;

	public LanternModel() {
		models = CCModel.parseObjModels("/mods/projectred/textures/obj/lantern.obj", new InvertX());
		for (CCModel c : models.values()) {
			c.computeNormals();
			c.apply(new Translation(.5, 0, .5));
		}

	}

	public void bindTextureForColorAndState(int color, boolean on) {
		String base = "/mods/projectred/textures/blocks/";
		String folder = on ? "lanternon/" : "lanternoff/";
		String file = EnumLantern.get(color).unlocalName + (on ? "on" : "off");
		String loc = base + folder + file + ".png";
		CCRenderState.changeTexture(loc);
	}

	protected void bindTextureByName(String par1Str) {
		RenderEngine renderengine = Minecraft.getMinecraft().renderEngine;
		if (renderengine != null) {
			renderengine.bindTexture(par1Str);
		}
	}

	public void renderLampBulb(double x, double y, double z, int rotation, boolean isOn) {
		GL11.glPushMatrix();
		CCRenderState.reset();
		CCRenderState.useNormals(true);
		CCRenderState.startDrawing(4);
		models.get("covertop").copy().apply(new Translation(x, y, z)).render();
		models.get("coverbottom").copy().apply(new Translation(x, y, z)).render();
		models.get("lamp").copy().apply(new Translation(x, y, z)).render();
		if (rotation == 0) {
			models.get("standbottom").copy().apply(new Translation(x, y, z)).render();
			models.get("goldringbottom").copy().apply(new Translation(x, y, z)).render();
		} else if (rotation == 1) {
			models.get("standtop").copy().apply(new Translation(x, y, z)).render();
			models.get("goldringtop").copy().apply(new Translation(x, y, z)).render();
		} else {
			models.get("standside").copy().apply(Rotation.getForSideFacing(0, rotation)).apply(new Translation(x, y, z)).render();
			models.get("goldringtop").copy().apply(Rotation.getForSideFacing(0, rotation)).apply(new Translation(x, y, z)).render();
		}
		CCRenderState.draw();
		GL11.glPopMatrix();
	}

	public void renderInventory(double x, double y, double z, float scale) {
		GL11.glPushMatrix();
		GL11.glTranslated(x, y, z);
		GL11.glScalef(scale, scale, scale);
		GL11.glRotatef(-90, 0f, 1f, 0f);
		CCRenderState.reset();
		CCRenderState.useNormals(true);
		CCRenderState.startDrawing(4);
		models.get("covertop").render();
		models.get("coverbottom").render();
		models.get("lamp").render();
		models.get("goldringtop").render();
		CCRenderState.draw();
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
		GL11.glTranslated(x, y, z);
		CCRenderState.reset();
		CCRenderState.useNormals(true);
		CCRenderState.startDrawing(4);
		models.get("lampshade").render();
		CCRenderState.draw();
		GL11.glPopMatrix();

		// Undo GL state changes
		GL11.glEnable(GL11.GL_TEXTURE_2D);
		GL11.glDisable(GL11.GL_BLEND);
		GL11.glEnable(GL11.GL_LIGHTING);
		GL11.glDepthMask(true);
	}
}
