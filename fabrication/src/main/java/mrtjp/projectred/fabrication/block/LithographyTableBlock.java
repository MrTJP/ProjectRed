package mrtjp.projectred.fabrication.block;

import mrtjp.projectred.fabrication.init.FabricationBlocks;
import mrtjp.projectred.fabrication.tile.LithographyTableTile;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.Nullable;

public class LithographyTableBlock extends FabricationMachineBlock {

    public LithographyTableBlock() {
        super(STONE_PROPERTIES);
    }

    @Nullable
    @Override
    public BlockEntity newBlockEntity(BlockPos pos, BlockState state) {
        return new LithographyTableTile(pos, state);
    }

    @Override
    protected BlockEntityType<?> getBlockEntityType() {
        return FabricationBlocks.LITHOGRAPHY_TABLE_TILE.get();
    }
}
