package mrtjp.projectred.expansion.block;

import mrtjp.projectred.expansion.tile.AutoCrafterTile;
import net.minecraft.block.BlockState;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.world.IBlockReader;

public class AutoCrafterBlock extends BaseMachineBlock {

    public AutoCrafterBlock() {
        super(WOODEN_PROPERTIES);
    }

    @Override
    protected TileEntity createTileEntityInstance(BlockState state, IBlockReader world) {
        return new AutoCrafterTile();
    }
}
