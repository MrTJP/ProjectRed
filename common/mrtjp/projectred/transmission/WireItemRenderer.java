package mrtjp.projectred.transmission;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.TextureUtils;
import codechicken.lib.vec.Scale;
import codechicken.lib.vec.Translation;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;
import net.minecraft.item.ItemStack;
import net.minecraftforge.client.IItemRenderer;

@SideOnly(Side.CLIENT)
public class WireItemRenderer implements IItemRenderer
{
    public final static WireItemRenderer instance = new WireItemRenderer();

    @Override
    public boolean handleRenderType(ItemStack item, ItemRenderType type)
    {
        return true;
    }

    @Override
    public boolean shouldUseRenderHelper(ItemRenderType type, ItemStack item, ItemRendererHelper helper)
    {
        return true;
    }

    @Override
    public void renderItem(ItemRenderType type, ItemStack item, Object... data)
    {
        int damage = item.getItemDamage();
        switch (type) {
        case ENTITY:
            renderWireInventory(damage, -.3f, 0f, -.3f, .6f);
            return;
        case EQUIPPED:
            renderWireInventory(damage, 0f, .15f, 0f, 1f);
            return;
        case EQUIPPED_FIRST_PERSON:
            renderWireInventory(damage, 1f, -.2f, -.4f, 2f);
            return;
        case INVENTORY:
            renderWireInventory(damage, 0f, .20f, 0f, 1f);
            return;
        default:
            return;
        }
    }

    public void renderWireInventory(int meta, float x, float y, float z, float scale)
    {
        WireDef type = WireDefs.VALID_WIRE()[meta];

        if (type == null)
            return;
        TextureUtils.bindAtlas(0);
        CCRenderState.reset();
        CCRenderState.useNormals(true);
        CCRenderState.pullLightmap();
        CCRenderState.setColourOpaque(type.itemColour());
        CCRenderState.startDrawing(7);
        RenderWire.renderInv(type.thickness(), new Scale(scale).with(new Translation(x, y, z)), type.wireSprites()[0]);
        CCRenderState.draw();
    }
}
