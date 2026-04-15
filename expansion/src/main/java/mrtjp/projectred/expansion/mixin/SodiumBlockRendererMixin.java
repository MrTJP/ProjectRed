package mrtjp.projectred.expansion.mixin;

import mrtjp.projectred.expansion.MovementManager;
import mrtjp.projectred.expansion.client.MovingBlockRenderManager;
import net.caffeinemc.mods.sodium.client.render.chunk.compile.ChunkBuildBuffers;
import net.caffeinemc.mods.sodium.client.render.chunk.compile.pipeline.BlockRenderer;
import net.caffeinemc.mods.sodium.client.render.chunk.translucent_sorting.TranslucentGeometryCollector;
import net.caffeinemc.mods.sodium.client.world.LevelSlice;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.block.state.BlockState;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

/**
 * Hooks into Sodium's render pipeline to conditionally disable rendering moving blocks. Works with
 * {@link SodiumAbstractBlockRenderContextMixin} to disable culling on moving sides.
 */
@Mixin(BlockRenderer.class)
public abstract class SodiumBlockRendererMixin extends SodiumAbstractBlockRenderContextMixin {

    @Inject(method = "prepare", at = @At("HEAD"))
    private void prepare(ChunkBuildBuffers buffers, LevelSlice level, TranslucentGeometryCollector collector, CallbackInfo ci) {
        cullDisabledSides.clear();
    }

    @Inject(method = "release", at = @At("HEAD"))
    private void release(CallbackInfo ci) {
        cullDisabledSides.clear();
    }

    @Inject(method = "renderModel", at = @At("HEAD"), cancellable = true)
    private void preRenderModel(BakedModel model, BlockState state, BlockPos pos, BlockPos origin, CallbackInfo ci) {
        // Fast path. If nothing is moving, bail
        MovementManager manager = MovementManager.getClientInstanceNullable();
        if (manager == null || manager.hasNoMovingStructures()) {
            return; // Proceeds to normal path
        }

        // If block is moving, don't render it
        if (MovingBlockRenderManager.isMoving(manager, pos)) {
            ci.cancel();
        }

        // If any neighbor is moving, disable culling on that side
        for (int s = 0; s < 6; s++) {
            Direction side = Direction.values()[s];
            var sidePos = pos.relative(side);
            if (MovingBlockRenderManager.isMoving(manager, sidePos)) {
                cullDisabledSides.add(side);
            }
        }
    }
}
