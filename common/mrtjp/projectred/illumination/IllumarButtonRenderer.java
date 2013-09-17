package mrtjp.projectred.illumination;

import net.minecraft.client.renderer.RenderBlocks;
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

public class IllumarButtonRenderer implements IItemRenderer {

    public static IllumarButtonRenderer instance = new IllumarButtonRenderer();
    
    Icon icon;
    
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
        icon = ItemPartIllumarButton.icons[item.getItemDamage()];
        switch (type) {
        case ENTITY:
            renderInventory(-.05f, 0, -.1f, .5f);
            return;
        case EQUIPPED:
            renderInventory(.2f, 0f, 0f, 1f);
            return;
        case EQUIPPED_FIRST_PERSON:
            renderInventory(.2f, 0f, 0f, 1f);
            return;
        case INVENTORY:
            renderInventory(.25f, 0, 0, 1f);
            return;
        default:
            return;
        }
    }

    private void renderInventory(float x, float y, float z, float scale) {
        RenderBlocks r = new RenderBlocks();
        Cuboid6 box = new Cuboid6(0.0, 0.375, 0.5 - 0.1875, 0.25, 0.625, 0.5 + 0.1875);
        GL11.glPushMatrix();
        GL11.glTranslated(x, y, z);
        GL11.glScalef(scale, scale, scale);

        TextureUtils.bindAtlas(0);
        CCRenderState.reset();
        CCRenderState.useNormals(true);
        CCRenderState.pullLightmap();
        CCRenderState.startDrawing(7);
        RenderUtils.renderBlock(box, 0, new Translation(x, y, z), new IconTransformation(icon), null);
        CCRenderState.draw();
        
        GL11.glPopMatrix();

    }
}
