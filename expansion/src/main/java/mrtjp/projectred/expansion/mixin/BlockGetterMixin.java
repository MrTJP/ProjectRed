package mrtjp.projectred.expansion.mixin;

import codechicken.lib.vec.Vector3;
import mrtjp.projectred.expansion.MovementManager;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.ClipContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.material.FluidState;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.Vec3;
import net.minecraft.world.phys.shapes.VoxelShape;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import javax.annotation.Nullable;
import java.util.LinkedList;
import java.util.List;

import static net.minecraft.world.level.BlockGetter.traverseBlocks;

@Mixin(BlockGetter.class)
public interface BlockGetterMixin {

    @Inject(method = "clip", at = @At(value = "HEAD"), cancellable = true)
    private void onClip(ClipContext clipContext, CallbackInfoReturnable<BlockHitResult> cir) {
        // Deal with movement only in active level
        if (!(this instanceof Level level)) return;

        MovementManager manager = MovementManager.getInstanceNullable(level);
        if (manager == null || manager.hasNoMovingStructures()) return;

        var hit = traverseBlocks(clipContext.getFrom(), clipContext.getTo(), clipContext, (ctx, pos) -> {

            var movementInfo = manager.getMovementInfo(pos);
            if (!movementInfo.isMoving()) {
                return lambda$clip$2(ctx, pos); // Super's impl of this lambda
            }

            Vec3 fromVec = ctx.getFrom();
            Vec3 toVec = ctx.getTo();

            List<BlockHitResult> hits = new LinkedList<>();

            // Hit test against block leaving the space
            Vector3 offset = movementInfo.getMovementOffset();
            gatherHits(level, ctx, fromVec, toVec, pos, pos, offset, hits);

            // Hit test against the incomming block
            Direction incomingDir = movementInfo.getDirection().getOpposite();
            BlockPos incomingPos = pos.relative(incomingDir);
            Vector3 incomingOffset = movementInfo.getMovementOffset().add(incomingDir.getNormal());
            gatherHits(level, ctx, fromVec, toVec, pos, incomingPos, incomingOffset, hits);

            return selectClosest(hits, fromVec);

        }, ctx -> {
            Vec3 vec3 = ctx.getFrom().subtract(ctx.getTo());
            return BlockHitResult.miss(ctx.getTo(), Direction.getNearest(vec3.x, vec3.y, vec3.z), BlockPos.containing(ctx.getTo()));
        });

        cir.setReturnValue(hit);
    }

    /**
     * Standard Block and fluid hit testing with option to provide a different block pos to provide the shapes from the
     * position the clip testing is done, and an optional shape offset.
     * <p>
     * Standard clip test behavior occurs when clipPos == blockPos and shapeOffset == (0,0,0).
     *
     * @param level       The level from which shapes will be queried via blockPos
     * @param ctx         The clip context
     * @param fromVec     Clip look vector start
     * @param toVec       Clip look vector end
     * @param clipPos     Block position of the clip in relation to fromVec and toVec
     * @param blockPos    Position of block providing the shapes (returned as position in BlockHitResult)
     * @param shapeOffset Offset to apply to the shape before hit testing
     * @param hits        List that receives the hits
     */
    @Unique
    private void gatherHits(Level level, ClipContext ctx, Vec3 fromVec, Vec3 toVec, BlockPos clipPos, BlockPos blockPos, Vector3 shapeOffset, List<BlockHitResult> hits) {
        BlockState blockstate = level.getBlockState(blockPos);
        VoxelShape blockShape = ctx.getBlockShape(blockstate, level, blockPos);
        VoxelShape interactShape = blockstate.getInteractionShape(level, blockPos);
        BlockHitResult blockHit = clipWithOffsetAndOverride(fromVec, toVec, clipPos, shapeOffset, blockShape, interactShape);
        if (blockHit != null) hits.add(blockHit.withPosition(blockPos));

        FluidState fluidstate = level.getFluidState(blockPos);
        VoxelShape fluidShape = ctx.getFluidShape(fluidstate, level, blockPos);
        BlockHitResult fluidHit = clipWithOffsetAndOverride(fromVec, toVec, clipPos, shapeOffset, fluidShape, null);
        if (fluidHit != null) hits.add(fluidHit.withPosition(blockPos));
    }

    @Unique
    @Nullable
    private BlockHitResult clipWithOffsetAndOverride(Vec3 from, Vec3 to, BlockPos pos, Vector3 offset, VoxelShape primaryShape, @Nullable VoxelShape override) {
        var offsetPrimary = primaryShape.move(offset.x, offset.y, offset.z);
        BlockHitResult hit = offsetPrimary.clip(from, to, pos);
        if (hit != null && override != null) {
            var offsetOverride = override.move(offset.x, offset.y, offset.z);
            BlockHitResult overrideHit = offsetOverride.clip(from, to, pos);
            if (overrideHit != null && overrideHit.getLocation().subtract(from).lengthSqr() < hit.getLocation().subtract(from).lengthSqr()) {
                // Primary hit with direction from override hit
                return hit.withDirection(overrideHit.getDirection());
            }
        }
        return hit;
    }

    @Nullable
    @Unique
    private BlockHitResult selectClosest(List<BlockHitResult> hits, Vec3 from) {
        if (hits.isEmpty()) return null;
        if (hits.size() == 1) return hits.getFirst();

        double closestDist = Double.MAX_VALUE;
        BlockHitResult closestHit = null;
        for (BlockHitResult hit : hits) {
            double dist = from.distanceToSqr(hit.getLocation());
            if (dist < closestDist) {
                closestDist = dist;
                closestHit = hit;
            }
        }
        return closestHit;
    }

    @Shadow
    private BlockHitResult lambda$clip$2(ClipContext clipContext, BlockPos pos) {
        //noinspection DataFlowIssue
        return null;
    }
}
