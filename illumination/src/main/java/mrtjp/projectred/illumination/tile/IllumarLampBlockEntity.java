package mrtjp.projectred.illumination.tile;

import codechicken.lib.vec.Cuboid6;
import mrtjp.projectred.illumination.BlockLightType;
import mrtjp.projectred.illumination.block.IllumarLampBlock;
import mrtjp.projectred.illumination.client.IllumarLampBlockEntityRenderer;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.AABB;

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

    @Override
    public AABB getRenderBoundingBox() {
        Cuboid6 c = IllumarLampBlockEntityRenderer.GLOW_BOUNDS.copy();
        return c.add(worldPosition).aabb();
    }
}
