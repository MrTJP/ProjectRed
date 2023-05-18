/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.api;

import net.minecraft.core.Direction;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;

/**
 * Interface for an object that manages the movement of blocks and tiles that are
 * registered to it. This class should be registered through the {@link IRelocationAPI}.
 */
public interface ITileMover {
    /**
     * Used to check if the block at the given position can move. This
     * method is only called if the specified block is tied to this
     * handler, so there is no need to check if the block is valid
     * for this handler. This is called before actually moving. If
     * everything is able to move, an animation will start.
     * <p>
     * Called server-side only when determining what to move.
     *
     * @param w The world.
     * @param pos The position of the block to move.
     * @return True if the block at the given position is able to move.
     */
    boolean canMove(Level w, BlockPos pos);

    /**
     * Method used to actually move the tile. Called after the animation
     * has run.  This is where you should tell the tile that it is time
     * to move, and peform any extra checks or calls. This should also
     * move the block and tile as well.  This is called
     * on every block in the moving structure sequentially.
     * <p>
     * Called on both server and client.
     *
     * @param w   The world.
     * @param pos The position of the block to move.
     * @param dir The ForgeDirection the structure is moving in.
     */
    void move(Level w, BlockPos pos, Direction dir);

    /**
     * Called after all blocks in the group have moved to their
     * new locations. This is where you would reload your tile,
     * tell it to refresh or reacknowledge its new position.
     * <p>
     * Callod on both server and client.
     *
     * @param w The world.
     * @param pos The position of the block to move.
     */
    void postMove(Level w, BlockPos pos);
}
