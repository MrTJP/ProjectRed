package mrtjp.projectred.core.block;

import codechicken.lib.vec.Rotation;
import mrtjp.projectred.core.init.CoreReferences;
import mrtjp.projectred.core.tile.ElectrotineGeneratorTile;
import net.minecraft.core.BlockPos;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;

import javax.annotation.Nullable;

public class ElectrotineGeneratorBlock extends ProjectRedBlock {

    public ElectrotineGeneratorBlock() {
        super(STONE_PROPERTIES);
    }

    @Nullable
    @Override
    public BlockEntity newBlockEntity(BlockPos pos, BlockState state) {
        return new ElectrotineGeneratorTile(pos, state);
    }

    @Override
    protected BlockEntityType<?> getBlockEntityType() {
        return CoreReferences.ELECTROTINE_GENERATOR_TILE;
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
