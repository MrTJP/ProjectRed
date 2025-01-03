package mrtjp.projectred.expansion.tile;

import codechicken.lib.vec.Rotation;
import mrtjp.projectred.expansion.init.ExpansionBlocks;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.state.BlockState;

public class FrameMotorBlockEntity extends BaseFrameMoverBlockEntity {

    public FrameMotorBlockEntity(BlockPos pos, BlockState state) {
        super(ExpansionBlocks.FRAME_MOTOR_BLOCK_ENTITY.get(), pos, state);
    }

    @Override
    protected int getFrontSide() {
        return getSide() ^ 1;
    }

    @Override
    protected int getMoveDir() {
        return Rotation.rotateSide(getSide(), (getRotation() + 2) % 4);
    }
}
