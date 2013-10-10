package mrtjp.projectred.core;

import java.util.Random;

import net.minecraft.block.Block;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;

public abstract class RenderMulti {

    protected Block block;

    public RenderMulti(Block b) {
        this.block = b;
    }
    
    public abstract void renderWorldBlock(RenderBlocks r, IBlockAccess w, int x, int y, int z, int meta);
    
    public abstract void renderInvBlock(RenderBlocks r, int meta);

    public abstract void randomDisplayTick(World w, int x, int y, int z, Random r);
}
