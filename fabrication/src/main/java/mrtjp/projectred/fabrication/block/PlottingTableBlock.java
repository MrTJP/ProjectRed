package mrtjp.projectred.fabrication.block;

import mrtjp.projectred.core.block.ProjectRedBlock;
import mrtjp.projectred.fabrication.init.FabricationReferences;
import mrtjp.projectred.fabrication.tile.PlottingTableTile;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.Nullable;

public class PlottingTableBlock extends FabricationMachineBlock {

    public PlottingTableBlock() {
        super(ProjectRedBlock.STONE_PROPERTIES);
    }

    @Nullable
    @Override
    public BlockEntity newBlockEntity(BlockPos pos, BlockState state) {
        return new PlottingTableTile(pos, state);
    }

    @Override
    protected BlockEntityType<?> getBlockEntityType() {
        return FabricationReferences.PLOTTING_TABLE_TILE;
    }
}
