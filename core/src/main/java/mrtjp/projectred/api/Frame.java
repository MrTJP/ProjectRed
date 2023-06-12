package mrtjp.projectred.api;

import net.minecraft.core.Direction;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;

import java.util.Collections;
import java.util.Set;

/**
 * Interface that can be implemented by blocks or block entities that wish to act as frames, which are the blocks
 * that stick together and form a moving structure. This interface is used during the resolution of connected
 * structures via the {@link IExpansionAPI#getStructure(Level, BlockPos, BlockPos...)} method. No action besides
 * implementation of this interface is needed.
 * <p>
 * Alternatively, you can use this as a block entity capability. The capability instance can be found at
 * {@link IExpansionAPI#FRAME_CAPABILITY}.
 * <p>
 * From order of most performant to least, the following are ways to add frame logic to a block:
 * 1. Implement this interface directly on block
 * 2. Implement this interface directly on block entity
 * 3. Add capability to block entity
 * 4. Register an {@link FrameInteraction} instance
 */
public interface Frame {

    /**
     * Used to check if this frame block is allowed to grab a block on the given side. Use this to conditionally
     * change a side's stickiness.
     * <p>
     * Called server-side only.
     *
     * @param w    The world.
     * @param pos  The block's position.
     * @param side The side of this block.
     * @return True if the side can grab another block.
     */
    boolean canGrab(Level w, BlockPos pos, Direction side);

    /**
     * Used to check if this block can be grabbed on the given side by another frame. Use this to prevent frames from
     * catching on a certain face. For example, a motor block may wish to not be grabbed on the face that moves frames.
     * <p>
     * Called server-side only.
     *
     * @param w    The world.
     * @param pos  The block's position.
     * @param side The side of this block.
     * @return True if the side can be grabbed by a frame block.
     */
    boolean canBeGrabbed(Level w, BlockPos pos, Direction side);

    /**
     * Optional method used to declare additional blocks that need to move along
     * with this block. Useful in situations where you want multiple non-adjacent
     * blocks to act as a single structure.
     *
     * @return Set of blocks that are stuck to this block.
     */
    default Set<BlockPos> getAdditionalSticks() {
        return Collections.emptySet();
    }
}
