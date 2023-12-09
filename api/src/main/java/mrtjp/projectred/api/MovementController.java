package mrtjp.projectred.api;

import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.Level;

/**
 * Interface that can be implemented by blocks or block entities that want to control when they can be moved
 * and to receive movement events.
 * <p>
 * Alternatively, you can use this as a block entity capability {@link IExpansionAPI#MOVEMENT_CONTROLLER_CAPABILITY}.
 */
public interface MovementController {

    /**
     * Called server-side before a move is executed. If any block in the structure returns false, the structure
     * will not be allowed to move.
     * <p>
     * Blocking frames from moving a block is heavily discouraged as it breaks the fundamental assumption of how frames
     * work. Ideally, frames should be able to move all blocks at all times. Use this interface only if absolutely
     * necessary.
     *
     * @param level Level
     * @param pos   Block position
     * @param dir   Movement direction
     * @return True to allow movement, false to block it
     */
    boolean isMovable(Level level, BlockPos pos, Direction dir);

    /**
     * Called server-side when the movement starts, when the animation starts playing.
     *
     * @param level Level
     * @param pos   Position
     * @param dir   Movement direction
     */
    void onMovementStarted(Level level, BlockPos pos, Direction dir);

    /**
     * Called server-side when the movement completes, and all blocks have moved over.
     *
     * @param level Level
     * @param pos   Position
     */
    void onMovementFinished(Level level, BlockPos pos);
}