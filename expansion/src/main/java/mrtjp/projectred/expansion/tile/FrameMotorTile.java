package mrtjp.projectred.expansion.tile;

import codechicken.lib.vec.Rotation;
import mrtjp.projectred.expansion.init.ExpansionReferences;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.state.BlockState;

public class FrameMotorTile extends BaseFrameMoverTile {

    public FrameMotorTile(BlockPos pos, BlockState state) {
        super(ExpansionReferences.FRAME_MOTOR_TILE, pos, state);
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
