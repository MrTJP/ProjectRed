/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.api;

import net.minecraft.core.Direction;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;

import java.util.Collections;
import java.util.Set;

/**
 * Interface that can be implemented by Blocks or by tile entities that wish to act as frames, which are the blocks
 * that stick together and form a moving structure when moved through the {@link Relocator}. No other action besides
 * implementation of this interface is needed for the block to function.
 *
 * Alternatively, you can use this as a tile entity capability. The capability instance can be found at
 * {@link IRelocationAPI#FRAME_CAPABILITY}.
 */
public interface IFrame
{
    /**
     * Used to check if this frame block is allowed to grab
     * a block on the given side.
     * <p>
     * Convention requires that this method yields the same result
     * on both client and server.
     *
     * @param w    The world.
     * @param pos  The block's position.
     * @param side The side of this block.
     * @return True if the side can grab another block.
     */
    boolean stickOut(Level w, BlockPos pos, Direction side);

    /**
     * Used to check if this frame block can be grabbed on the
     * given side by another frame.
     * <p>
     * Convention requires that this method yields the same result
     * on both client and server.
     *
     * @param w    The world.
     * @param pos  The block's position.
     * @param side The side of this block.
     * @return True if the side can be grabbed by a frame block.
     */
    boolean stickIn(Level w, BlockPos pos, Direction side);

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
