package mrtjp.projectred.illumination.client;

import codechicken.lib.vec.Cuboid6;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.core.BundledSignalsLib;
import mrtjp.projectred.core.client.HaloRenderer;
import mrtjp.projectred.illumination.tile.IllumarSmartLampBlockEntity;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.blockentity.BlockEntityRenderer;
import net.minecraft.world.phys.AABB;

public class IllumarSmartLampBlockEntityRenderer implements BlockEntityRenderer<IllumarSmartLampBlockEntity> {

    public static final IllumarSmartLampBlockEntityRenderer INSTANCE = new IllumarSmartLampBlockEntityRenderer();

    public static final Cuboid6 GLOW_BOUNDS = Cuboid6.full.copy().expand(0.05D);

    @Override
    public void render(IllumarSmartLampBlockEntity tile, float pPartialTick, PoseStack pPoseStack, MultiBufferSource pBufferSource, int pPackedLight, int pPackedOverlay) {

        byte[] signal = tile.getSignal();
        if (!BundledSignalsLib.isSignalZero(signal)) {
            HaloRenderer.addMultiLight(tile.getBlockPos(), GLOW_BOUNDS, tile.getSignal());
        }
    }

    @Override
    public int getViewDistance() {
        return 256;
    }

    @Override
    public AABB getRenderBoundingBox(IllumarSmartLampBlockEntity blockEntity) {
        return GLOW_BOUNDS
                .copy()
                .add(blockEntity.getBlockPos())
                .aabb();
    }
}
