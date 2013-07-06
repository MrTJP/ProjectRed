package mrtjp.projectred.utils;

import static net.minecraftforge.client.IItemRenderer.ItemRenderType.ENTITY;
import static net.minecraftforge.client.IItemRenderer.ItemRendererHelper.BLOCK_3D;
import net.minecraft.block.Block;
import net.minecraft.client.renderer.OpenGlHelper;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.entity.RenderItem;
import net.minecraft.client.renderer.entity.RenderManager;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.client.renderer.texture.TextureMap;
import net.minecraft.entity.item.EntityItem;
import net.minecraft.item.ItemBlock;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.client.IItemRenderer;
import net.minecraftforge.client.MinecraftForgeClient;

import org.lwjgl.opengl.GL11;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public class BasicRenderUtils {

	public static void setBrightness(IBlockAccess w, int x, int y, int z) {
		Tessellator.instance.setBrightness(w.getLightBrightnessForSkyBlocks(x, y, z, 0));
	}

	public static void setBrightnessDirect(IBlockAccess w, int x, int y, int z) {
		int i = w.getLightBrightnessForSkyBlocks(x, y, z, 0);
		OpenGlHelper.setLightmapTextureCoords(OpenGlHelper.lightmapTexUnit, i & 0xFFFF, i >> 16);
	}

	public static void setFullBrightness() {
		Tessellator.instance.setBrightness(0x00F000F0);
	}

	public static void renderCube(double x, double y, double z, double cx, double cy, double cz, int tex_top, int tex_bottom, int tex_side, double du, double dv) {
		Tessellator t = Tessellator.instance;
		float u, v;

		u = (tex_bottom & 15) / 16.0f;
		v = (tex_bottom / 16) / 16.0f;

		t.setNormal(0.0F, -1.0F, 0.0F);
		t.addVertexWithUV(x, y, z, u, v);
		t.addVertexWithUV(x + cx, y, z, u + du, v);
		t.addVertexWithUV(x + cx, y, z + cz, u + du, v + dv);
		t.addVertexWithUV(x, y, z + cz, u, v + dv);

		u = (tex_side & 15) / 16.0f;
		v = (tex_side / 16) / 16.0f;

		t.setNormal(0.0F, 0.0F, -1.0F);
		t.addVertexWithUV(x + cx, y + cy, z, u, v);
		t.addVertexWithUV(x + cx, y, z, u, v + dv);
		t.addVertexWithUV(x, y, z, u + du, v + dv);
		t.addVertexWithUV(x, y + cy, z, u + du, v);

		t.setNormal(0.0F, 0.0F, 1.0F);
		t.addVertexWithUV(x + cx, y + cy, z + cz, u, v);
		t.addVertexWithUV(x, y + cy, z + cz, u + du, v);
		t.addVertexWithUV(x, y, z + cz, u + du, v + dv);
		t.addVertexWithUV(x + cx, y, z + cz, u, v + dv);

		t.setNormal(-1.0F, 0.0F, 0.0F);
		t.addVertexWithUV(x, y + cy, z, u, v);
		t.addVertexWithUV(x, y, z, u, v + dv);
		t.addVertexWithUV(x, y, z + cz, u + du, v + dv);
		t.addVertexWithUV(x, y + cy, z + cz, u + du, v);

		t.setNormal(1.0F, 0.0F, 0.0F);
		t.addVertexWithUV(x + cx, y + cy, z, u, v);
		t.addVertexWithUV(x + cx, y + cy, z + cz, u + du, v);
		t.addVertexWithUV(x + cx, y, z + cz, u + du, v + dv);
		t.addVertexWithUV(x + cx, y, z, u, v + dv);

		u = (tex_top & 15) / 16.0f;
		v = (tex_top / 16) / 16.0f;

		t.setNormal(0.0F, 1.0F, 0.0F);
		t.addVertexWithUV(x, y + cy, z, u, v);
		t.addVertexWithUV(x, y + cy, z + cz, u, v + dv);
		t.addVertexWithUV(x + cx, y + cy, z + cz, u + du, v + dv);
		t.addVertexWithUV(x + cx, y + cy, z, u + du, v);
	}

	public static Icon[] loadIconArray(IconRegister reg, String prefix, int num) {
		Icon[] rv = new Icon[num];
		for (int k = 0; k < num; k++)
			rv[k] = loadIcon(reg, prefix + k);
		return rv;
	}

	public static Icon loadIcon(IconRegister reg, String name) {
		if (reg instanceof TextureMap) {
			((TextureMap) reg).setTextureEntry(name, name.contains(">") ? new TextureSlice(name) : new TextureStitchedNonSquare(name));
		}
		return reg.registerIcon(name);
	}

	static EntityItem entityItem;
	static RenderItem uniformRenderItem = new RenderItem() {
		@Override
		public boolean shouldBob() {
			return false;
		}
	};

	static {
		uniformRenderItem.setRenderManager(RenderManager.instance);
		entityItem = new EntityItem(null);
		entityItem.hoverStart = 0;
	}

	public static void renderItemUniform(ItemStack item) {
		IItemRenderer customRenderer = MinecraftForgeClient.getItemRenderer(item, ENTITY);
		boolean is3D = customRenderer != null && customRenderer.shouldUseRenderHelper(ENTITY, item, BLOCK_3D);

		boolean larger = false;
		if (item.getItem() instanceof ItemBlock && RenderBlocks.renderItemIn3d(Block.blocksList[item.itemID].getRenderType())) {
			int renderType = Block.blocksList[item.itemID].getRenderType();
			larger = !(renderType == 1 || renderType == 19 || renderType == 12 || renderType == 2);
		} else if (is3D) {
			larger = true;
		}

		double d = 2;
		double d1 = 1 / d;
		if (larger)
			GL11.glScaled(d, d, d);

		GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F);

		entityItem.setEntityItemStack(item);
		uniformRenderItem.doRenderItem(entityItem, 0, larger ? 0.09 : 0.06, 0, 0, 0);

		if (larger)
			GL11.glScaled(d1, d1, d1);
	}

}