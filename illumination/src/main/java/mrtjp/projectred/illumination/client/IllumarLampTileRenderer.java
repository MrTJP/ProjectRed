package mrtjp.projectred.illumination.client;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.projectred.core.client.HaloRenderer;
import mrtjp.projectred.illumination.block.IllumarLampBlock;
import mrtjp.projectred.illumination.tile.IllumarLampTile;
import net.minecraft.block.BlockState;
import net.minecraft.client.renderer.IRenderTypeBuffer;
import net.minecraft.client.renderer.tileentity.TileEntityRenderer;
import net.minecraft.client.renderer.tileentity.TileEntityRendererDispatcher;

public class IllumarLampTileRenderer extends TileEntityRenderer<IllumarLampTile> {

    private static final Cuboid6 GLOW_BOUNDS = Cuboid6.full.copy().expand(0.05D);

    public IllumarLampTileRenderer(TileEntityRendererDispatcher dispatcher) {
        super(dispatcher);
    }

    @Override
    public void render(IllumarLampTile tile, float partialTicks, MatrixStack matrixStack, IRenderTypeBuffer buffers, int combinedLight, int combinedOverlay) {

        int g = (tile.getBlockPos().getX() + tile.getBlockPos().getY() + tile.getBlockPos().getZ()) % 4;

//        Cuboid6 bounds = sh ? GLOW_BOUNDS.copy().expand(0.0001D) : GLOW_BOUNDS;

        Cuboid6 bounds = GLOW_BOUNDS.copy().expand(0.0001D * g);

        if (tile.getLevel() != null) {
            BlockState state = tile.getLevel().getBlockState(tile.getBlockPos());
            if (state.getBlock() instanceof IllumarLampBlock && tile.isLit()) {
                HaloRenderer.renderHalo(CCRenderState.instance(), matrixStack, buffers, bounds, tile.color, Vector3.ZERO);
            }
        }
    }
}
