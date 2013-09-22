package mrtjp.projectred.illumination;

import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraftforge.client.IItemRenderer;

import org.lwjgl.opengl.GL11;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.IconTransformation;
import codechicken.lib.render.RenderUtils;
import codechicken.lib.render.TextureUtils;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Translation;

public class RenderIllumarButton implements IItemRenderer {

    public static RenderIllumarButton instance = new RenderIllumarButton();
    
    private static Cuboid6 invRenderBox = new Cuboid6(0.0, 0.375, 0.5 - 0.1875, 0.25, 0.625, 0.5 + 0.1875);
    private static Cuboid6 invLightBox = invRenderBox.copy().expand(0.025D);

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
        switch (type) {
        case ENTITY:
            renderInventory(item.getItemDamage(), -.05f, 0, -.1f, .5f);
            return;
        case EQUIPPED:
            renderInventory(item.getItemDamage(), .2f, 0f, 0f, 1f);
            return;
        case EQUIPPED_FIRST_PERSON:
            renderInventory(item.getItemDamage(), .2f, 0f, 0f, 1f);
            return;
        case INVENTORY:
            renderInventory(item.getItemDamage(), .25f, 0, 0, 1f);
            return;
        default:
            return;
        }
    }

    private void renderInventory(int color, float x, float y, float z, float scale) {
        Icon icon = ItemPartIllumarButton.icons[color];

        GL11.glPushMatrix();
        GL11.glTranslated(x, y, z);
        GL11.glScalef(scale, scale, scale);

        TextureUtils.bindAtlas(0);
        CCRenderState.reset();
        CCRenderState.useNormals(true);
        CCRenderState.pullLightmap();
        CCRenderState.startDrawing(7);
        RenderUtils.renderBlock(invRenderBox, 0, new Translation(x, y, z), new IconTransformation(icon), null);
        CCRenderState.draw();
        RenderHalo.prepareRenderState();
        RenderHalo.renderHalo(Tessellator.instance, invLightBox, color,  new Translation(x, y, z));
        RenderHalo.restoreRenderState();
        GL11.glPopMatrix();

    }
}
