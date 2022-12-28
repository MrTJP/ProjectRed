package mrtjp.projectred.illumination.tile;

import mrtjp.projectred.illumination.BlockLightType;
import mrtjp.projectred.illumination.block.IllumarLampBlock;
import net.minecraft.tileentity.TileEntity;

public class IllumarLampTile extends TileEntity {

    public final int color;
    public final boolean inverted;

    public IllumarLampTile(int color, boolean inverted) {
        super(BlockLightType.ILLUMAR_LAMP.getTileEntityType(color, inverted));
        this.color = color;
        this.inverted = inverted;
    }

    public boolean isLit() {
        return level.getBlockState(getBlockPos()).getValue(IllumarLampBlock.LIT);
    }
}
