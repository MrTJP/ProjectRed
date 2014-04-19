package mrtjp.projectred.core.libmc;

import cpw.mods.fml.client.registry.ISimpleBlockRenderingHandler;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;
import mrtjp.projectred.core.PRLogger;
import net.minecraft.block.Block;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.world.IBlockAccess;

@SideOnly(Side.CLIENT)
public class BasicRenderUtils
{
    public static int coreRenderHandlerID = -1;

    private static IndexedRender[] renderers = new IndexedRender[4096];

    private static class IndexedRender
    {
        public MultiTileRender[] indexedRenders = new MultiTileRender[16];
        MultiTileRender defaultRender;

        public int damage(int meta)
        {
            return meta;
        }
    }

    public static void setRenderer(Block b, int meta, MultiTileRender render)
    {
        if (renderers[Block.blockRegistry.getIDForObject(b)] == null)
        {
            renderers[Block.blockRegistry.getIDForObject(b)] = new IndexedRender();
        }
        renderers[Block.blockRegistry.getIDForObject(b)].indexedRenders[meta] = render;
    }

    public static MultiTileRender getInvRenderer(int id, int meta)
    {
        IndexedRender ir = renderers[id];
        if (ir == null)
            return null;
        int damage = ir.damage(meta);
        if (damage > 15)
            return ir.defaultRender;
        return ir.indexedRenders[ir.damage(meta)];
    }

    public static MultiTileRender getRenderer(Block b, int meta)
    {
        IndexedRender ir = renderers[Block.getIdFromBlock(b)];
        if (ir == null)
            return null;
        return ir.indexedRenders[meta];
    }

    public static class MultiRenderHandler implements ISimpleBlockRenderingHandler
    {
        public static MultiRenderHandler instance = new MultiRenderHandler();

        public void renderInventoryBlock(Block b, int md, int modelID, RenderBlocks r)
        {
            if (modelID != coreRenderHandlerID)
                return;

            MultiTileRender render = BasicRenderUtils.getInvRenderer(Block.blockRegistry.getIDForObject(b), md);

            if (render == null)
            {
                PRLogger.severe("No render mapping found for "+Block.blockRegistry.getIDForObject(b)+":"+md);
                return;
            }

            render.renderInvBlock(r, md);
        }

        public boolean renderWorldBlock(IBlockAccess w, int x, int y, int z, Block b, int modelId, RenderBlocks r)
        {
            if (r.overrideBlockTexture != null)
                return true;

            if (modelId != coreRenderHandlerID)
                return false;

            int meta = w.getBlockMetadata(x, y, z);
            MultiTileRender render = BasicRenderUtils.getRenderer(b, meta);

            if (render == null)
            {
                PRLogger.severe("No render mapping found for "+b.getUnlocalizedName()+":"+meta);
                return true;
            }

            render.renderWorldBlock(r, w, x, y, z, meta);
            return true;
        }

        public boolean shouldRender3DInInventory(int meta)
        {
            return true;
        }

        public int getRenderId()
        {
            return coreRenderHandlerID;
        }
    }

}