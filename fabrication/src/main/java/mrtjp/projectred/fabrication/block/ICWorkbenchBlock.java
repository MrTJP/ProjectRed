package mrtjp.projectred.fabrication.block;

import mrtjp.projectred.core.block.ProjectRedBlock;
import mrtjp.projectred.fabrication.init.FabricationBlocks;
import mrtjp.projectred.fabrication.tile.ICWorkbenchBlockEntity;
import net.minecraft.core.BlockPos;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.properties.BooleanProperty;
import org.jetbrains.annotations.Nullable;

public class ICWorkbenchBlock extends ProjectRedBlock {

    public static final BooleanProperty BLUEPRINT_PROPERTY = BooleanProperty.create("blueprint");

    public ICWorkbenchBlock() {
        super(STONE_PROPERTIES);
    }

    @Nullable
    @Override
    public BlockState getStateForPlacement(BlockPlaceContext context) {
        return defaultBlockState()
                .setValue(BLUEPRINT_PROPERTY, false);
    }

    @Nullable
    @Override
    public BlockEntity newBlockEntity(BlockPos pos, BlockState state) {
        return new ICWorkbenchBlockEntity(pos, state);
    }

    @Override
    protected BlockEntityType<?> getBlockEntityType() {
        return FabricationBlocks.IC_WORKBENCH_BLOCK_ENTITY.get();
    }

    @Override
    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        builder.add(BLUEPRINT_PROPERTY);
    }
}
