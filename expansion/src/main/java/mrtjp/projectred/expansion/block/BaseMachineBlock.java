package mrtjp.projectred.expansion.block;

import codechicken.lib.vec.Rotation;
import mrtjp.projectred.core.block.ProjectRedBlock;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;

import javax.annotation.Nullable;

public abstract class BaseMachineBlock extends ProjectRedBlock {

    public BaseMachineBlock(Properties properties) {
        super(properties);
    }

    @Nullable
    @Override
    public BlockState getStateForPlacement(BlockPlaceContext context) {
        int r = Rotation.rotationTo(0, context.getHorizontalDirection().ordinal());
        return this.defaultBlockState()
                .setValue(ROTATION, r)
                .setValue(CHARGED, false)
                .setValue(WORKING, false);
    }

    @Override
    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        builder.add(ROTATION);
        builder.add(CHARGED);
        builder.add(WORKING);
    }
}
