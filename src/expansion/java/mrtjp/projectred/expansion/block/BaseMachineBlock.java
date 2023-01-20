package mrtjp.projectred.expansion.block;

import codechicken.lib.vec.Rotation;
import mrtjp.projectred.core.block.ProjectRedBlock;
import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.item.BlockItemUseContext;
import net.minecraft.state.StateContainer;

import javax.annotation.Nullable;

public abstract class BaseMachineBlock extends ProjectRedBlock {

    public BaseMachineBlock(Properties properties) {
        super(properties);
    }

    @Nullable
    @Override
    public BlockState getStateForPlacement(BlockItemUseContext context) {
        int r = Rotation.rotationTo(0, context.getHorizontalDirection().ordinal());
        return this.defaultBlockState()
                .setValue(ROTATION, r)
                .setValue(CHARGED, false)
                .setValue(WORKING, false);
    }

    @Override
    protected void createBlockStateDefinition(StateContainer.Builder<Block, BlockState> builder) {
        builder.add(ROTATION);
        builder.add(CHARGED);
        builder.add(WORKING);
    }
}
