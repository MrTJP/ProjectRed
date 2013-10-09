package mrtjp.projectred.integration;

import net.minecraft.item.ItemStack;
import net.minecraftforge.client.IItemRenderer;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.TextureUtils;
import codechicken.lib.vec.Scale;
import codechicken.lib.vec.Translation;

public class GateItemRenderer implements IItemRenderer
{
    public final static GateItemRenderer instance = new GateItemRenderer();
    
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
            renderGateInv(damage, -0.3f, 0f, -0.3f, 0.6f);
            return;
        case EQUIPPED:
            renderGateInv(damage, 0f, 0.15f, 0f, 1f);
            return;
        case EQUIPPED_FIRST_PERSON:
            renderGateInv(damage, 1f, -0.2f, -0.4f, 2f);
            return;
        case INVENTORY:
            renderGateInv(damage, 0f, 0.20f, 0f, 1f);
            return;
        default:
            return;
        }
    }

    public void renderGateInv(int meta, float x, float y, float z, float scale) {
        if(!EnumGate.VALID_GATES[meta].implemented())
            return;
        
        TextureUtils.bindAtlas(0);
        CCRenderState.reset();
        CCRenderState.useNormals(true);
        CCRenderState.pullLightmap();
        CCRenderState.setColourOpaque(-1);
        RenderGate.renderInv(new Scale(scale).with(new Translation(x, y, z)), meta);
    }
}
