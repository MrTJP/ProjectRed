package mrtjp.projectred.illumination;

import mrtjp.projectred.core.BasicRenderUtils;
import mrtjp.projectred.core.BasicUtils;
import net.minecraft.block.Block;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.client.IItemRenderer;

import org.lwjgl.opengl.GL11;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.IconTransformation;
import codechicken.lib.render.RenderUtils;
import codechicken.lib.render.TextureUtils;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Translation;
import cpw.mods.fml.client.registry.ISimpleBlockRenderingHandler;

public class LampISBRH implements ISimpleBlockRenderingHandler, IItemRenderer {

    public static LampISBRH instance = new LampISBRH();

    private static Cuboid6 box = new Cuboid6(0, 0, 0, 1, 1, 1).expand(0.05D);

    @Override
    public void renderInventoryBlock(Block block, int metadata, int modelID, RenderBlocks renderer) {}

    @Override
    public boolean renderWorldBlock(IBlockAccess world, int x, int y, int z, Block block, int modelId, RenderBlocks r) {
        TileLamp l = BasicUtils.getTileEntity(world, new BlockCoord(x,y,z), TileLamp.class);
        if (l == null)
            return false;

        TextureUtils.bindAtlas(0);

        if (l.isOn())
            BasicRenderUtils.setFullBrightness();

        r.renderStandardBlock(block, x, y, z);

        if (l.isOn())
            RenderHalo.addLight(x, y, z, world.getBlockMetadata(x, y, z), 6, box);
        return true;
    }

    @Override
    public boolean shouldRender3DInInventory() {
        return true;
    }

    @Override
    public int getRenderId() {
        return IlluminationClientProxy.lampRenderID;
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
            return;
        }
    }

    private void renderInventory(int meta, double x, double y, double z, double scale) {
        Icon icon = meta>15 ? BlockLamp.onIcons[meta-16] : BlockLamp.offIcons[meta];
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
        if(meta > 15) {
            RenderHalo.prepareRenderState();
            RenderHalo.renderHalo(Tessellator.instance, box, meta-16,  new Translation(x, y, z));
            RenderHalo.restoreRenderState();
        }
        GL11.glPopMatrix();
    }

}
