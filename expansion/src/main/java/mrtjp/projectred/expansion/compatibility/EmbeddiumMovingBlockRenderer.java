package mrtjp.projectred.expansion.compatibility;

import com.mojang.blaze3d.vertex.VertexConsumer;
import mrtjp.projectred.expansion.MovementManager;
import mrtjp.projectred.expansion.client.MovingBlockRenderManager;
import net.minecraft.client.Minecraft;
import net.minecraft.util.RandomSource;
import org.embeddedt.embeddium.api.BlockRendererRegistry;
import org.embeddedt.embeddium.api.render.chunk.BlockRenderContext;

/**
 * Render hook for moving blocks for Embeddium render pipeline
 */
public class EmbeddiumMovingBlockRenderer implements BlockRendererRegistry.Renderer {

    @Override
    public BlockRendererRegistry.RenderResult renderBlock(BlockRenderContext ctx, RandomSource randomSource, VertexConsumer vertexConsumer) {

        var blockRenderer = Minecraft.getInstance().getBlockRenderer();

        // Fast path. If nothing is moving, bail
        MovementManager manager = MovementManager.getClientInstanceNullable();
        if (manager == null || manager.hasNoMovingStructures()) {
            return BlockRendererRegistry.RenderResult.PASS;
        }

        // If block is moving, don't render it
        if (MovingBlockRenderManager.isMoving(manager, ctx.pos())) {
            return BlockRendererRegistry.RenderResult.OVERRIDE;
        }

        // If block is adjacent to moving block, render without culling
        if (MovingBlockRenderManager.isAdjacentToMoving(manager, ctx.pos())) {
            blockRenderer.renderBatched(ctx.state(), ctx.pos(), ctx.localSlice(), ctx.stack(), vertexConsumer, false, randomSource, ctx.modelData(), ctx.renderLayer());
            return BlockRendererRegistry.RenderResult.OVERRIDE;
        }

        return BlockRendererRegistry.RenderResult.PASS;
    }
}
