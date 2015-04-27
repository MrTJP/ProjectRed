package coloredlightscore.src.api;

import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.world.IBlockAccess;

public abstract class CLBlock extends Block {

    public CLBlock(Material matt) {
        super(matt);
    }

    public abstract int getColorLightValue(int metadata);

    @Override
    public int getLightValue(IBlockAccess world, int x, int y, int z) {
        return getColorLightValue(world.getBlockMetadata(x, y, z));
    }
}
