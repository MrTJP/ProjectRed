package mrtjp.projectred.core;

import mrtjp.projectred.core.BlockBasics.EnumBasics;
import net.minecraft.block.Block;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.world.IBlockAccess;
import cpw.mods.fml.client.registry.ISimpleBlockRenderingHandler;

public class BasicsISBRH implements ISimpleBlockRenderingHandler {

    @Override
    public void renderInventoryBlock(Block block, int meta, int modelID, RenderBlocks renderer) {
        if (meta == EnumBasics.ALLOYSMELTER.meta)
            RenderAlloySmelter.renderInventory();
    }

    @Override
    public boolean renderWorldBlock(IBlockAccess world, int x, int y, int z, Block block, int modelId, RenderBlocks renderer) {
        TileEntity t = world.getBlockTileEntity(x, y, z);
        if (t instanceof TileAlloySmelter)
            RenderAlloySmelter.renderStatic(world, x, y, z, block, modelId, renderer);
        
        return true;
    }

    @Override
    public boolean shouldRender3DInInventory() {
        return true;
    }

    @Override
    public int getRenderId() {
        return CoreClientProxy.basicRenderID;
    }

}
