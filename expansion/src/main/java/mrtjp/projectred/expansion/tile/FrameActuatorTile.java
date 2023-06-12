package mrtjp.projectred.expansion.tile;

import mrtjp.projectred.expansion.init.ExpansionReferences;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.state.BlockState;

public class FrameActuatorTile extends BaseFrameMoverTile {

    public FrameActuatorTile(BlockPos pos, BlockState state) {
        super(ExpansionReferences.FRAME_ACTUATOR_TILE, pos, state);
    }

    @Override
    protected int getFrontSide() {
        return getSide() ^ 1;
    }

    @Override
    protected int getMoveDir() {
        return getSide() ^ 1;
    }
}
