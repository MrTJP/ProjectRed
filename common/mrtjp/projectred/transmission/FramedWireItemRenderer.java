package mrtjp.projectred.transmission;

import net.minecraft.item.ItemStack;
import net.minecraftforge.client.IItemRenderer;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.TextureUtils;
import codechicken.lib.vec.Scale;
import codechicken.lib.vec.Translation;

public class FramedWireItemRenderer implements IItemRenderer {

    public static FramedWireItemRenderer instance = new FramedWireItemRenderer();
    
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
        int damage = item.getItemDamage();
        switch (type) {
        case ENTITY:
            renderWireInventory(damage, -.5f, 0f, -.5f, 1f);
            return;
        case EQUIPPED:
            renderWireInventory(damage, 0f, .0f, 0f, 1f);
            return;
        case EQUIPPED_FIRST_PERSON:
            renderWireInventory(damage, 1f, -.6f, -.4f, 2f);
            return;
        case INVENTORY:
            renderWireInventory(damage, 0f, -.1f, 0f, 1f);
            return;
        default:
            return;
        }
    }

    public void renderWireInventory(int meta, float x, float y, float z, float scale) {
        EnumWire type = EnumWire.VALID_WIRE[meta];
        if (type == null)
            return;
        TextureUtils.bindAtlas(0);
        CCRenderState.reset();
        CCRenderState.useNormals(true);
        CCRenderState.pullLightmap();
        CCRenderState.setColourOpaque(type.itemColour);
        CCRenderState.startDrawing(7);
        RenderFramedWire.renderInv(type.thickness, new Scale(scale).with(new Translation(x, y, z)), type.wireSprites[0]);
        CCRenderState.draw();
    }

}
