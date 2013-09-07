package mrtjp.projectred.illumination;

import java.util.Map;

import mrtjp.projectred.ProjectRedIllumination;
import mrtjp.projectred.core.BasicRenderUtils;
import mrtjp.projectred.core.InvertX;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.client.IItemRenderer;
import net.minecraftforge.client.model.IModelCustom;

import org.lwjgl.opengl.GL11;

import codechicken.lib.render.CCModel;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.IUVTransformation;
import codechicken.lib.render.IconTransformation;
import codechicken.lib.render.RenderUtils;
import codechicken.lib.render.TextureUtils;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.TransformationList;
import codechicken.lib.vec.Translation;

public class LampRenderer implements IItemRenderer {

   
    public static LampRenderer instance = new LampRenderer();

    private Icon lampIcon;

    public Icon bindTexture(LampPart lamp) {
        return lamp.getLightValue() == 15 ? lamp.type.onIcon : lamp.type.offIcon;
    }

    public void renderLamp(LampPart lamp, RenderBlocks b) {
        lampIcon = b == null || b.overrideBlockTexture == null ? bindTexture(lamp) : b.overrideBlockTexture; 
        CCRenderState.reset();
        TextureUtils.bindAtlas(0);
        CCRenderState.setBrightness(lamp.world(), lamp.x(), lamp.y(), lamp.z());
        renderLampBulb(lamp.x(), lamp.y(), lamp.z());
        if (lamp.getLightValue() == 15 && b == null)
            renderLampShade(lamp.x(), lamp.y(), lamp.z(), lamp.type.meta);        
    }
        
    public void renderLampBulb(double x, double y, double z) {
        RenderUtils.renderBlock(Cuboid6.full, 0, new Translation(x, y, z), new IconTransformation(lampIcon), null);
    }

    public void renderLampShade(int x, int y, int z, int tint) {
        Cuboid6 box = new Cuboid6(0, 0, 0, 1, 1, 1).expand(0.05D);
        LastEventBasedHaloRenderer.addLight(x, y, z, tint, 6, box);
    }
    
    public void renderInventory(double x, double y, double z, float scale) {
        GL11.glPushMatrix();
        GL11.glTranslated(x, y, z);
        GL11.glScalef(scale, scale, scale);
        TextureUtils.bindAtlas(0);
        CCRenderState.reset();
        CCRenderState.useNormals(true);
        CCRenderState.pullLightmap();
        CCRenderState.startDrawing(7);
        renderLampBulb(x, y, z);
        CCRenderState.draw();

        GL11.glPopMatrix();
    }


    @Override
    public boolean handleRenderType(ItemStack item, ItemRenderType type) {
        return true;
    }

    @Override
    public boolean shouldUseRenderHelper(ItemRenderType type, ItemStack item, ItemRendererHelper helper) {
        return true;
    }

    @Override
    public void renderItem(ItemRenderType type, ItemStack item, Object... data) {
        lampIcon = item.getItem() == ProjectRedIllumination.itemPartInvLamp ? EnumLamp.get(item.getItemDamage()).onIcon : EnumLamp.get(item.getItemDamage()).offIcon;
        switch (type) {
        case ENTITY:
            renderInventory(-.15f, 0f, -.15f, .50f);
            return;
        case EQUIPPED:
            renderInventory(0f, 0f, 0f, 1f);
            return;
        case EQUIPPED_FIRST_PERSON:
            renderInventory(0f, 0f, 0f, 1f);
            return;
        case INVENTORY:
            renderInventory(0f, -.05f, 0f, 1f);
            return;
        default:
            return;
        }

    }
}
