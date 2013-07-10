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
}
