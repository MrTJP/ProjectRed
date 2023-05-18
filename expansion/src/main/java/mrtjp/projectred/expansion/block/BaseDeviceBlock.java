package mrtjp.projectred.expansion.block;

import mrtjp.projectred.core.block.ProjectRedBlock;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import org.jetbrains.annotations.Nullable;

public abstract class BaseDeviceBlock extends ProjectRedBlock {

    public BaseDeviceBlock(Properties properties) {
        super(properties);
    }

    @Nullable
    @Override
    public BlockState getStateForPlacement(BlockPlaceContext context) {
        int s = context.getClickedFace().ordinal() ^ 1; // Place bottom of block on the side clicked
        return defaultBlockState()
                .setValue(SIDE, s)
                .setValue(ACTIVE, false);
    }

    @Override
    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        super.createBlockStateDefinition(builder);
        builder.add(SIDE, ACTIVE);
    }
}
