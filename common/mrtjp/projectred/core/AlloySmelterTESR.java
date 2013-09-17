package mrtjp.projectred.core;

import net.minecraft.client.renderer.tileentity.TileEntitySpecialRenderer;
import net.minecraft.tileentity.TileEntity;

public class AlloySmelterTESR extends TileEntitySpecialRenderer {

    @Override
    public void renderTileEntityAt(TileEntity tileentity, double x, double y, double z, float frame) {
        RenderAlloySmelter.renderDynamic(x, y, z, frame, (TileAlloySmelter)tileentity);
    }
}
