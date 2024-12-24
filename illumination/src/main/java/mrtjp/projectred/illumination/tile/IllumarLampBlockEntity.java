package mrtjp.projectred.illumination.tile;

import mrtjp.projectred.illumination.BlockLightType;
import mrtjp.projectred.illumination.block.IllumarLampBlock;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;

public class IllumarLampBlockEntity extends BlockEntity {

    public final int color;
    public final boolean inverted;

    public IllumarLampBlockEntity(int color, boolean inverted, BlockPos pos, BlockState state) {
        super(BlockLightType.ILLUMAR_LAMP.getTileEntityType(color, inverted), pos, state);
        this.color = color;
        this.inverted = inverted;
    }

    public boolean isLit() {
        return level.getBlockState(getBlockPos()).getValue(IllumarLampBlock.LIT);
    }
}
