package mrtjp.projectred.expansion.block;

import mrtjp.projectred.core.tile.IBlockEventTile;
import mrtjp.projectred.expansion.tile.FireStarterTile;
import net.minecraft.block.BlockState;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.Direction;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockReader;
import net.minecraft.world.IWorldReader;

public class FireStarterBlock extends BaseDeviceBlock {

    public FireStarterBlock() {
        super(STONE_PROPERTIES);
    }

    @Override
    protected TileEntity createTileEntityInstance(BlockState state, IBlockReader world) {
        return new FireStarterTile();
    }

    @Override
    public boolean isFireSource(BlockState state, IWorldReader world, BlockPos pos, Direction side) {
        if (super.isFireSource(state, world, pos, side)) return true;

        TileEntity tile = world.getBlockEntity(pos);
        if (tile instanceof IBlockEventTile) return ((IBlockEventTile) tile).isFireSource(side.ordinal());
        return false;
    }
}
