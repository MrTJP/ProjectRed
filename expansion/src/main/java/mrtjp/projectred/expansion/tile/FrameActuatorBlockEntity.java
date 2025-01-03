package mrtjp.projectred.expansion.tile;

import mrtjp.projectred.expansion.init.ExpansionBlocks;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.state.BlockState;

public class FrameActuatorBlockEntity extends BaseFrameMoverBlockEntity {

    public FrameActuatorBlockEntity(BlockPos pos, BlockState state) {
        super(ExpansionBlocks.FRAME_ACTUATOR_BLOCK_ENTITY.get(), pos, state);
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
