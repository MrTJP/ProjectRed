package mrtjp.projectred.api;

import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.Level;

/**
 * Interface for an object that manages the movement of blocks and tiles that are
 * registered to it. This class should be registered through the {@link IExpansionAPI}.
 * <p>
 * The order of calls are:
 * <ol>
 *     <li>Server receives {@link #canMove(Level, BlockPos)} </li>
 *     <li>Server receives {@link #move(Level, BlockPos, Direction)}</li>
 *     <li>Client receives {@link #move(Level, BlockPos, Direction)}</li>
 *     <li>Client receives {@link #postMove(Level, BlockPos)}</li>
 *     <li>Server receives {@link #postMove(Level, BlockPos)}</li>
 * </ol>
 */
public interface BlockMover {

    /**
     * Used to check if the block at the given position can move. This
     * method is only called if the specified block is tied to this
     * handler, so there is no need to check if the block is valid
     * for this handler. This is called before actually moving. If
     * everything is able to move, an animation will start.
     * <p>
     * Called server-side only.
     *
     * @param w   The world.
     * @param pos The position of the block before movement.
     * @return True if the block at the given position is able to move.
     */
    boolean canMove(Level w, BlockPos pos);

    /**
     * Method used to actually move the tile. Called after the animation
     * has run. This is where you should tell the tile that it is time
     * to move, and perform any extra checks or calls. This should also
     * move the block and tile as well. This is called on every block in
     * the moving structure sequentially.
     * <p>
     * Called on server then client. Note that after server call, block
     * positions of client/server are out of sync. Do not send packets
     * to client referencing blocks by position, update neighbors, mark
     * chunk for re-render, etc.
     *
     * @param w   The world.
     * @param pos The position of the block before movement.
     * @param dir The ForgeDirection the structure is moving in.
     */
    void move(Level w, BlockPos pos, Direction dir);

    /**
     * Called after all blocks in the group have moved to their new locations.
     * This is where you would reload your tile, tell it to refresh or
     * re-acknowledge its new position. No need to update neighbors, since
     * they are batch-notified after all blocks (moved and adjacent to moved)
     * get postMove call.
     * <p>
     * Called on client then server. At this point, both client and server
     * blocks are in sync at new positions. It is safe to send packets
     * with position references.
     *
     * @param w   The world.
     * @param pos The position of the block after movement.
     */
    void postMove(Level w, BlockPos pos);
}
