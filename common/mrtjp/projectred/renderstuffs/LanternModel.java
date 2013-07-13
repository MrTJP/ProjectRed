package mrtjp.projectred.renderstuffs;

import java.util.Map;

import mrtjp.projectred.blocks.BlockLantern.EnumLantern;
import mrtjp.projectred.core.ProjectRedTickHandler;
import mrtjp.projectred.multipart.wiring.RotatedTessellator;
import mrtjp.projectred.tiles.TileLantern;
import mrtjp.projectred.utils.Color;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.RenderEngine;
import net.minecraft.client.renderer.Tessellator;
import net.minecraftforge.client.model.AdvancedModelLoader;
import net.minecraftforge.client.model.IModelCustom;

import org.lwjgl.opengl.GL11;

import codechicken.core.render.CCModel;
import codechicken.core.render.CCRenderState;
import codechicken.core.vec.SwapYZ;
import codechicken.core.vec.Translation;

public class LanternModel {

	private Map<String, CCModel> models;

	public LanternModel() {
		models = CCModel.parseObjModels("/mods/projectred/textures/obj/lantern.obj", new SwapYZ());
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

	public void renderLampBulb(double x, double y, double z, int rotation, boolean isOn, TileLantern tile) {
		GL11.glPushMatrix();
		GL11.glDisable(GL11.GL_LIGHTING);
		RotatedTessellator rt = new RotatedTessellator();
		rt.base = Tessellator.instance;
		rt.flipped = false;
		rt.front = rotation;
		rt.side = 0;
		rt.x = x;
		rt.y = y;
		rt.z = z;
		CCRenderState.reset();
		CCRenderState.useNormals(true);
		CCRenderState.startDrawing(4);
		models.get("covertop").renderWithSpecial(rt);
		models.get("coverbottom").renderWithSpecial(rt);
		models.get("lamp").renderWithSpecial(rt);
		if (rotation == 0) {
			models.get("standbottom").renderWithSpecial(rt);
			models.get("goldringbottom").renderWithSpecial(rt);
		} else if (rotation == 1) {
			models.get("standtop").renderWithSpecial(rt);
			models.get("goldringtop").renderWithSpecial(rt);
		} else {
			models.get("standside").renderWithSpecial(rt);
			models.get("goldringtop").renderWithSpecial(rt);
		}
		CCRenderState.draw();
		GL11.glEnable(GL11.GL_LIGHTING);
		GL11.glPopMatrix();
	}

	public void renderInventory(double x, double y, double z, float scale) {
		GL11.glPushMatrix();
		GL11.glTranslated(x, y, z);
		GL11.glScalef(scale, scale, scale);
		GL11.glRotatef(-90, 0f, 1f, 0f);
		GL11.glDisable(GL11.GL_LIGHTING);
		CCRenderState.reset();
		CCRenderState.useNormals(true);
		CCRenderState.startDrawing(4);
		models.get("covertop").render();
		models.get("coverbottom").render();
		models.get("lamp").render();
		models.get("goldringtop").render();
		CCRenderState.draw();
		GL11.glEnable(GL11.GL_LIGHTING);
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
