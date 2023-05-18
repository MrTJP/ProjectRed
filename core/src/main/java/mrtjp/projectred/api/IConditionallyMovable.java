package mrtjp.projectred.api;

import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;

/**
 * Interface that can be implemented by Blocks or by tile entities that have wish to have their own logic for when
 * they can be moved by frames. Alternatively, you can use this as a tile entity capability. The capability instance
 * can be found at {@link IRelocationAPI#CONDITIONALLY_MOVABLE_CAPABILITY}.
 *
 * Blocking frames from moving a block is heavily discouraged as it breaks the fundamental assumption of how frames
 * work. Ideally, frames should be able to move all blocks at all times. Use this interface only if absolutely
 * necessary.
 */
public interface IConditionallyMovable
{
    /**
     * Called before a move is executed. If any block in the structure returns false, the structure
     * will not be allowed to move.
     *
     * @param w    The world.
     * @param pos  The block's position.
     * @return True if this block is in a movable state.
     */
    boolean isMovable(Level w, BlockPos pos);
}