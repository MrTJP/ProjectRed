package mrtjp.projectred.expansion.block;

import codechicken.lib.vec.Rotation;
import mrtjp.projectred.core.block.ProjectRedBlock;
import mrtjp.projectred.expansion.init.ExpansionBlocks;
import mrtjp.projectred.expansion.tile.FrameMotorBlockEntity;
import net.minecraft.core.BlockPos;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import org.jetbrains.annotations.Nullable;

public class FrameMotorBlock extends ProjectRedBlock {

    public FrameMotorBlock() {
        super(STONE_MACHINE_PROPERTIES);
    }

    @Nullable
    @Override
    public BlockEntity newBlockEntity(BlockPos pos, BlockState state) {
        return new FrameMotorBlockEntity(pos, state);
    }

    @Override
    protected BlockEntityType<?> getBlockEntityType() {
        return ExpansionBlocks.FRAME_MOTOR_BLOCK_ENTITY.get();
    }

    @Nullable
    @Override
    public BlockState getStateForPlacement(BlockPlaceContext context) {
        int s = context.getClickedFace().ordinal() ^ 1; // Place bottom of block on the side clicked
        int r = context.getPlayer() == null ? 0 : (Rotation.getSidedRotation(context.getPlayer(), context.getClickedFace().ordinal()) + 2) % 4;
        return defaultBlockState()
                .setValue(SIDE, s)
                .setValue(ROTATION, r)
                .setValue(CHARGED, false)
                .setValue(WORKING, false);
    }

    @Override
    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        super.createBlockStateDefinition(builder);
        builder.add(SIDE, ROTATION, CHARGED, WORKING);
    }
}
