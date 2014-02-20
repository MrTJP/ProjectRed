package mrtjp.projectred.illumination;

import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.tileentity.TileEntitySpecialRenderer;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.Icon;
import net.minecraftforge.client.IItemRenderer;

import org.lwjgl.opengl.GL11;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.IconTransformation;
import codechicken.lib.render.RenderUtils;
import codechicken.lib.render.TextureUtils;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Translation;

public class LampTESR extends TileEntitySpecialRenderer implements IItemRenderer
{
    public static LampTESR instance = new LampTESR();
    private static Cuboid6 box = Cuboid6.full.copy().expand(0.05D);

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
        switch (type) {
        case ENTITY:
            renderInventory(item.getItemDamage(), -0.15f, 0f, -0.15f, 1f);
            return;
        case EQUIPPED:
            renderInventory(item.getItemDamage(), 0f, 0f, 0f, 1f);
            return;
        case EQUIPPED_FIRST_PERSON:
            renderInventory(item.getItemDamage(), 0f, 0f, 0f, 1f);
            return;
        case INVENTORY:
            renderInventory(item.getItemDamage(), 0f, -0.05f, 0f, 0.95f);
            return;
        default:
        }
    }

    private void renderInventory(int meta, double x, double y, double z, double scale)
    {
        Icon icon = meta > 15 ? BlockLamp.onIcons[meta - 16] : BlockLamp.offIcons[meta];
        GL11.glPushMatrix();
        GL11.glTranslated(x, y, z);
        GL11.glScaled(scale, scale, scale);
        TextureUtils.bindAtlas(0);
        CCRenderState.reset();
        CCRenderState.useNormals(true);
        CCRenderState.pullLightmap();
        CCRenderState.startDrawing(7);
        RenderUtils.renderBlock(Cuboid6.full, 0, new Translation(x, y, z), new IconTransformation(icon), null);
        CCRenderState.draw();
        if (meta > 15)
        {
            RenderHalo.prepareRenderState();
            RenderHalo.renderHalo(Tessellator.instance, box, meta - 16, new Translation(x, y, z));
            RenderHalo.restoreRenderState();
        }
        GL11.glPopMatrix();
    }

    @Override
    public void renderTileEntityAt(TileEntity te, double x, double y, double z, float f)
    {
        if (te instanceof ILight && ((ILight)te).isOn())
        {
            int meta = te.worldObj.getBlockMetadata(te.xCoord, te.yCoord, te.zCoord);
            RenderHalo.addLight(te.xCoord, te.yCoord, te.zCoord, meta, box);
        }
    }
}
