package mrtjp.projectred.expansion.client;

import codechicken.lib.render.block.ICCBlockRenderer;
import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import mrtjp.projectred.expansion.MovementManager;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.util.RandomSource;
import net.minecraft.world.level.BlockAndTintGetter;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.client.model.data.ModelData;
import org.jetbrains.annotations.Nullable;

/**
 * A global block renderer that takes care of temporarily suppressing rendering for any blocks that are
 * currently moving, so the moving block renderer can take over.
 */
public class MovingBlockSuppressorRenderer implements ICCBlockRenderer {

    public static final MovingBlockSuppressorRenderer INSTANCE = new MovingBlockSuppressorRenderer();

    public static boolean allowMovingRenderOnRenderThread = false;

    private static boolean isRendering = false;

    private MovingBlockSuppressorRenderer() {
    }

    @Override
    public boolean canHandleBlock(BlockAndTintGetter world, BlockPos pos, BlockState blockState, @Nullable RenderType renderType) {
        // Infinite recursion prevention. #renderBlock is attempting a no-cull render.
        if (isRendering) return false;

        // Take over both moving and adjacent to moving blocks
        boolean isMoving = isMoving(pos);

        if (isMoving) {
            // If we're on render thread and force flag is set, it means we're rendering the moving block with an offset.
            // So return false here to prevent this from taking over and suppressing the render
            if (RenderSystem.isOnRenderThread() && allowMovingRenderOnRenderThread) {
                return false;
            }
            // Otherwise take over to block rendering
            return true;
        }

        // If adjacent to a moving block, take over to render without culling
        return isAdjacentToMoving(pos);
    }

    @Override
    public void renderBlock(BlockState state, BlockPos pos, BlockAndTintGetter world, PoseStack mStack, VertexConsumer builder, RandomSource random, ModelData data, @Nullable RenderType renderType) {

        // Moving blocks don't render here
        if (isMoving(pos)) return;

        // Render adjacent blocks without culling
        isRendering = true; // Prevents ourselves from re-handling this event
        // renderType should be nullable
        //noinspection DataFlowIssue
        Minecraft.getInstance().getBlockRenderer().renderBatched(state, pos, world, mStack, builder, false, random, data, renderType);
        isRendering = false;
    }

    private static boolean isMoving(BlockPos pos) {
        if (Minecraft.getInstance().level == null) return false;
        return MovementManager.getInstance(Minecraft.getInstance().level).getMovementInfo(pos).isMoving();
    }

    private static boolean isAdjacentToMoving(BlockPos pos) {
        for (int s = 0; s < 6; s++) {
            if (isMoving(pos.relative(Direction.values()[s]))) return true;
        }
        return false;
    }
}
