package mrtjp.projectred.fabrication.block;

import mrtjp.projectred.fabrication.init.FabricationBlocks;
import mrtjp.projectred.fabrication.tile.PackagingTableBlockEntity;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.Nullable;

public class PackagingTableBlock extends FabricationMachineBlock {

    public PackagingTableBlock() {
        super(STONE_PROPERTIES);
    }

    @Nullable
    @Override
    public BlockEntity newBlockEntity(BlockPos pos, BlockState state) {
        return new PackagingTableBlockEntity(pos, state);
    }

    @Override
    protected BlockEntityType<?> getBlockEntityType() {
        return FabricationBlocks.PACKAGING_TABLE_BLOCK_ENTITY.get();
    }
}
