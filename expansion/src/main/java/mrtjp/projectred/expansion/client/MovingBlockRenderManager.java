package mrtjp.projectred.expansion.client;

import codechicken.lib.render.RenderUtils;
import codechicken.lib.render.buffer.TransformingVertexConsumer;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import mrtjp.projectred.expansion.MovementManager;
import mrtjp.projectred.expansion.MovingStructure;
import mrtjp.projectred.expansion.MovingStructureInfo;
import net.minecraft.client.Camera;
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
import net.minecraft.world.phys.shapes.VoxelShape;
import net.neoforged.api.distmarker.Dist;
import net.neoforged.api.distmarker.OnlyIn;
import net.neoforged.neoforge.client.event.RenderHighlightEvent;
import net.neoforged.neoforge.client.event.RenderLevelStageEvent;
import net.neoforged.neoforge.client.model.data.ModelData;

import java.util.Iterator;
import java.util.List;
import java.util.Objects;

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

    @OnlyIn(Dist.CLIENT)
    public static void onDrawHighlight(RenderHighlightEvent.Block event) {
        // Check for movement. Return if not moving
        MovementManager manager = MovementManager.getClientInstanceNullable();
        if (manager == null || manager.hasNoMovingStructures()) return;
        MovingStructureInfo info = manager.getMovementInfo(event.getTarget().getBlockPos());
        if (!info.isMoving()) return;

        Level level = Objects.requireNonNull(Minecraft.getInstance().level);
        BlockPos pos = event.getTarget().getBlockPos();
        BlockState state = level.getBlockState(pos);
        if (state.isAir() || !level.getWorldBorder().isWithinBounds(pos)) return;

        Camera camera = event.getCamera();
        PoseStack pStack = event.getPoseStack();
        pStack.pushPose();
        pStack.translate(-camera.getPosition().x, -camera.getPosition().y, -camera.getPosition().z);

        VoxelShape shape = state.getShape(level, pos);
        var offset = info.getRenderOffset(event.getDeltaTracker().getGameTimeDeltaPartialTick(false));
        pStack.translate(pos.getX(), pos.getY(), pos.getZ());
        pStack.translate(offset.x, offset.y, offset.z);
        VertexConsumer consumer = new TransformingVertexConsumer(event.getMultiBufferSource().getBuffer(RenderType.lines()), pStack);
        RenderUtils.bufferShapeOutline(consumer, shape, 0, 0, 0, 0.4F); // RGBA from LevelRenderer#renderHitOutline

        pStack.popPose();

        event.setCanceled(true);
    }
}
