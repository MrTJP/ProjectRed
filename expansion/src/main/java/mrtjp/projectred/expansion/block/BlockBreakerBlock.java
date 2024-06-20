package mrtjp.projectred.expansion.block;

import mrtjp.projectred.expansion.init.ExpansionBlocks;
import mrtjp.projectred.expansion.tile.BlockBreakerBlockEntity;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.Nullable;

public class BlockBreakerBlock extends BaseDeviceBlock {

    public BlockBreakerBlock() {
        super(STONE_PROPERTIES);
    }

    @Nullable
    @Override
    public BlockEntity newBlockEntity(BlockPos pPos, BlockState pState) {
        return new BlockBreakerBlockEntity(pPos, pState);
    }

    @Override
    protected BlockEntityType<?> getBlockEntityType() {
        return ExpansionBlocks.BLOCK_BREAKER_TILE.get();
    }
}
