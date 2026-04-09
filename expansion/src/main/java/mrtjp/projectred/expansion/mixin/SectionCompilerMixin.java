package mrtjp.projectred.expansion.mixin;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import mrtjp.projectred.expansion.MovementManager;
import mrtjp.projectred.expansion.client.MovingBlockRenderManager;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.block.BlockRenderDispatcher;
import net.minecraft.client.renderer.chunk.SectionCompiler;
import net.minecraft.core.BlockPos;
import net.minecraft.util.RandomSource;
import net.minecraft.world.level.BlockAndTintGetter;
import net.minecraft.world.level.block.state.BlockState;
import net.neoforged.neoforge.client.model.data.ModelData;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

/**
 * Render hook for moving blocks for Minecraft's default render pipeline
 */
@Mixin(SectionCompiler.class)
public class SectionCompilerMixin {

    @Redirect(
            method = "compile(Lnet/minecraft/core/SectionPos;Lnet/minecraft/client/renderer/chunk/RenderChunkRegion;Lcom/mojang/blaze3d/vertex/VertexSorting;Lnet/minecraft/client/renderer/SectionBufferBuilderPack;Ljava/util/List;)Lnet/minecraft/client/renderer/chunk/SectionCompiler$Results;",
            at = @At(
                    value = "INVOKE",
                    target = "net/minecraft/client/renderer/block/BlockRenderDispatcher.renderBatched (Lnet/minecraft/world/level/block/state/BlockState;Lnet/minecraft/core/BlockPos;Lnet/minecraft/world/level/BlockAndTintGetter;Lcom/mojang/blaze3d/vertex/PoseStack;Lcom/mojang/blaze3d/vertex/VertexConsumer;ZLnet/minecraft/util/RandomSource;Lnet/neoforged/neoforge/client/model/data/ModelData;Lnet/minecraft/client/renderer/RenderType;)V"
            )
    )
    private static void compile_WrapRenderBatched(
            BlockRenderDispatcher blockRenderer,
            BlockState state,
            BlockPos pos,
            BlockAndTintGetter blockAndTintGetter,
            PoseStack poseStack,
            VertexConsumer buffer,
            boolean enableCulling,
            RandomSource random,
            ModelData modelData,
            RenderType renderType) {

        MovementManager manager = MovementManager.getClientInstanceNullable();

        if (manager != null && !manager.hasNoMovingStructures()) {
            // If block is moving, don't render it
            if (MovingBlockRenderManager.isMoving(manager, pos)) {
                return;
            }

            // If block is adjacent to moving block, render without culling
            if (MovingBlockRenderManager.isAdjacentToMoving(manager, pos)) {
                blockRenderer.renderBatched(state, pos, blockAndTintGetter, poseStack, buffer, false, random, modelData, renderType);
                return;
            }
        }

        // Render normally
        blockRenderer.renderBatched(state, pos, blockAndTintGetter, poseStack, buffer, enableCulling, random, modelData, renderType);
    }
}
