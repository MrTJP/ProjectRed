package mrtjp.projectred.core;

import static net.minecraftforge.client.IItemRenderer.ItemRenderType.ENTITY;
import static net.minecraftforge.client.IItemRenderer.ItemRendererHelper.BLOCK_3D;
import net.minecraft.block.Block;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.OpenGlHelper;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.entity.RenderItem;
import net.minecraft.client.renderer.entity.RenderManager;
import net.minecraft.client.renderer.texture.TextureMap;
import net.minecraft.entity.item.EntityItem;
import net.minecraft.item.ItemBlock;
import net.minecraft.item.ItemStack;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.client.IItemRenderer;
import net.minecraftforge.client.MinecraftForgeClient;

import org.lwjgl.opengl.GL11;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public class BasicRenderUtils {
    
    public static int currentRenderPass = 0;

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

    public static void setFullColor() {
        Tessellator.instance.setColorRGBA(255, 255, 255, 255);
    }
}