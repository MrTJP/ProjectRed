package mrtjp.projectred.core;

import mrtjp.projectred.core.blockutil.RenderMulti;
import net.minecraft.block.Block;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.world.IBlockAccess;
import cpw.mods.fml.client.registry.ISimpleBlockRenderingHandler;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public class BasicRenderUtils
{
    public static int coreRenderHandlerID = -1;

    private static IndexedRender[] renderers = new IndexedRender[4096];

    private static class IndexedRender
    {
        public RenderMulti[] indexedRenders = new RenderMulti[16];
        RenderMulti defaultRender;

        public int damage(int meta)
        {
            return meta;
        }
    }

    private static RenderMulti makeRenderer(Block bl, Class renderer)
    {
        try
        {
            return (RenderMulti) renderer.getDeclaredConstructor(new Class[] { Block.class }).newInstance(new Object[] { bl });
        } catch (Exception e)
        {
            e.printStackTrace();
        }
        return null;
    }

    public static void setRenderer(Block b, int meta, Class<? extends RenderMulti> renderer)
    {
        RenderMulti render = makeRenderer(b, renderer);
        if (renderers[b.blockID] == null)
        {
            renderers[b.blockID] = new IndexedRender();
        }
        renderers[b.blockID].indexedRenders[meta] = render;
    }

    public static RenderMulti getInvRenderer(int id, int meta)
    {
        IndexedRender ir = renderers[id];
        if (ir == null)
            return null;
        int damage = ir.damage(meta);
        if (damage > 15)
            return ir.defaultRender;
        return ir.indexedRenders[ir.damage(meta)];
    }

    public static RenderMulti getRenderer(int id, int meta)
    {
        IndexedRender ir = renderers[id];
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

            RenderMulti render = BasicRenderUtils.getInvRenderer(b.blockID, md);

            if (render == null)
            {
                PRLogger.severe("No render mapping found for " + b.blockID + ":" + md);
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
            RenderMulti render = BasicRenderUtils.getRenderer(b.blockID, meta);

            if (render == null)
            {
                PRLogger.severe("No render mapping found for " + b.blockID + ":" + meta);
                return true;
            }

            render.renderWorldBlock(r, w, x, y, z, meta);
            return true;
        }

        public boolean shouldRender3DInInventory()
        {
            return true;
        }

        public int getRenderId()
        {
            return coreRenderHandlerID;
        }
    }

}