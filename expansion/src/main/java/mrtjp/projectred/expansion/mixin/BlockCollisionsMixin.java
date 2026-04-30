package mrtjp.projectred.expansion.mixin;

import codechicken.lib.vec.Vector3;
import mrtjp.projectred.expansion.MovementManager;
import mrtjp.projectred.expansion.MovingStructureInfo;
import mrtjp.projectred.expansion.ProjectRedExpansion;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.BlockCollisions;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.shapes.CollisionContext;
import net.minecraft.world.phys.shapes.Shapes;
import net.minecraft.world.phys.shapes.VoxelShape;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

@Mixin(BlockCollisions.class)
public abstract class BlockCollisionsMixin<T> {

    @Redirect(
            method = "computeNext",
            at = @At(
                    value = "INVOKE",
                    target = "net/minecraft/world/level/block/state/BlockState.getCollisionShape (Lnet/minecraft/world/level/BlockGetter;Lnet/minecraft/core/BlockPos;Lnet/minecraft/world/phys/shapes/CollisionContext;)Lnet/minecraft/world/phys/shapes/VoxelShape;"
            )
    )
    private VoxelShape modifyCollisionShapeForMovingBlocks(
            BlockState state,
            BlockGetter getter,
            BlockPos pos,
            CollisionContext context) {

        VoxelShape shape = state.getCollisionShape(getter, pos, context);

        if (!(getter instanceof Level level)) return shape;

        MovementManager manager = MovementManager.getInstanceNullable(level);
        if (manager == null) return shape;

        MovingStructureInfo info = manager.getMovementInfo(pos);
        if (!info.isMoving()) return shape;

        // Merge in adjacent block's shape if moving in same direction
        var adjacentOffset = info.getDirection().getOpposite();
        BlockPos pos2 = pos.relative(info.getDirection().getOpposite());
        MovingStructureInfo info2 = manager.getMovementInfo(pos2);
        if (info2.isMoving() && info2.getDirection() == info.getDirection()) {
            var shape2 = level.getBlockState(pos2).getCollisionShape(level, pos2, context);
            shape2 = shape2.move(adjacentOffset.getStepX(), adjacentOffset.getStepY(), adjacentOffset.getStepZ());
            var shape3 = Shapes.or(shape, shape2);
            ProjectRedExpansion.LOGGER.info("{}@{} adjacent block merged: {} + {} = {}", state, pos, shape, shape2, shape3);
            shape = shape3;
        } else {
            ProjectRedExpansion.LOGGER.info("{}@{} adjacent block not moving: {}", state, pos, shape);
        }

        // Apply offset to shape
        Vector3 offset = info.getMovementOffset();
        return shape.move(offset.x, offset.y, offset.z);
    }
}