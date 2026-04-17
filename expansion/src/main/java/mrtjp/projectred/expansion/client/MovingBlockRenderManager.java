package mrtjp.projectred.expansion.client;

import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.expansion.MovementManager;
import mrtjp.projectred.expansion.MovingStructure;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.block.BlockRenderDispatcher;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.util.RandomSource;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.Vec3;
import net.neoforged.api.distmarker.Dist;
import net.neoforged.api.distmarker.OnlyIn;
import net.neoforged.neoforge.client.event.RenderLevelStageEvent;
import net.neoforged.neoforge.client.model.data.ModelData;

import java.util.Iterator;
import java.util.List;

public class MovingBlockRenderManager {

    public static boolean isMoving(MovementManager manager, BlockPos pos) {
        return manager.getMovementInfo(pos).isMoving();
    }

    public static boolean isAdjacentToMoving(MovementManager manager, BlockPos pos) {
        for (int s = 0; s < 6; s++) {
            var rPos = pos.relative(Direction.values()[s]);
            if (manager.getMovementInfo(rPos).isMoving()) return true;
        }
        return false;
    }

    @OnlyIn(Dist.CLIENT)
    public static void onRenderLevelStage(RenderLevelStageEvent event) {
        Level level = Minecraft.getInstance().level;
        if (level == null) return;

        MovementManager manager = MovementManager.getInstance(level);
        if (manager.hasNoMovingStructures()) return;

        // Get the renderType for this stage, and skip if we dont care about it
        List<RenderType> renderTypes = List.of(RenderType.solid(), RenderType.cutout(), RenderType.cutoutMipped(), RenderType.translucent());
        RenderType renderType = null;
        for (RenderType type : renderTypes) {
            if (RenderLevelStageEvent.Stage.fromRenderType(type) == event.getStage()) {
                renderType = type;
                break;
            }
        }
        if (renderType == null) return;

        RandomSource random = RandomSource.create();

        // Set up camera pose
        Vec3 cam = event.getCamera().getPosition();
        PoseStack stack = event.getPoseStack();
        stack.pushPose();
        stack.mulPose(event.getModelViewMatrix());
        stack.translate(-cam.x, -cam.y, -cam.z);

        for (MovingStructure structure : manager.getMovingStructures()) {

            // Set up render offset based on progress of movement
            Vector3 offset = structure.getRenderOffset(event.getPartialTick().getGameTimeDeltaPartialTick(false));
            stack.pushPose();
            stack.translate(offset.x, offset.y, offset.z);

            MultiBufferSource.BufferSource buffers = Minecraft.getInstance().renderBuffers().bufferSource();
            BlockRenderDispatcher blockRenderer = Minecraft.getInstance().getBlockRenderer();

            Iterator<BlockPos> it = structure.iteratePreMove();
            while (it.hasNext()) {
                BlockPos p = it.next();
                BlockState state = level.getBlockState(p);
                BakedModel model = blockRenderer.getBlockModel(state);
                ModelData data = level.getModelData(p);

                if (!model.getRenderTypes(state, random, data).contains(renderType)) {
                    continue;
                }

                // Render the moving block
                stack.pushPose();
                stack.translate(p.getX(), p.getY(), p.getZ());
                blockRenderer.renderBatched(state, p, level, stack, buffers.getBuffer(renderType), false, random, data, renderType);
                stack.popPose(); //p
            }

            buffers.endBatch();
            stack.popPose(); //offset
        }

        stack.popPose(); //cam
    }
}
