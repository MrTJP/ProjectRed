package mrtjp.projectred.illumination.client;

import codechicken.lib.vec.Cuboid6;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.core.client.HaloRenderer;
import mrtjp.projectred.illumination.block.IllumarLampBlock;
import mrtjp.projectred.illumination.tile.IllumarLampBlockEntity;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.blockentity.BlockEntityRenderer;
import net.minecraft.world.level.block.state.BlockState;

public class IllumarLampBlockEntityRenderer implements BlockEntityRenderer<IllumarLampBlockEntity> {

    public static final IllumarLampBlockEntityRenderer INSTANCE = new IllumarLampBlockEntityRenderer();

    public static final Cuboid6 GLOW_BOUNDS = Cuboid6.full.copy().expand(0.05D);

    private IllumarLampBlockEntityRenderer() {
    }

    @Override
    public void render(IllumarLampBlockEntity tile, float partialTicks, PoseStack matrixStack, MultiBufferSource buffers, int combinedLight, int combinedOverlay) {

        if (tile.getLevel() != null) {
            BlockState state = tile.getLevel().getBlockState(tile.getBlockPos());
            if (state.getBlock() instanceof IllumarLampBlock && tile.isLit()) {
                HaloRenderer.addLight(tile.getBlockPos(), GLOW_BOUNDS, tile.color);
            }
        }
    }

    @Override
    public int getViewDistance() {
        return 256;
    }
}
