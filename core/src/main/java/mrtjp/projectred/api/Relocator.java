/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.api;

import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;

import java.util.Set;

public abstract class Relocator {
    /**
     * Pushes an instance of the Relocator onto the stack.
     * Must be called before using any methods.
     */
    public abstract void push();

    /**
     * Pops the instance of the Relocator off the stack.
     * Must be called after you are done using this.
     *
     * @throws IllegalStateException If there is no Relocator on the stack.
     */
    public abstract void pop();

    /**
     * Sets the world for the movement.
     *
     * @param world The world in which the movement will occure.
     * @throws IllegalStateException If the world was already set.
     */
    public abstract void setWorld(Level world);

    /**
     * Sets the direction towards which all the queued blocks will move.
     * This is a ForgeDirection index, from 0 to 5
     *
     * @param dir The direction to move the blocks.
     * @throws IllegalStateException If the Relocator is not on the stack.
     * @throws IllegalStateException If the direction was already set.
     */
    public abstract void setDirection(int dir);

    /**
     * Sets the speed of the movement. Must be greater than 0 and
     * less than 1.
     *
     * @param speed The speed of the movement, in meters/tick.
     * @throws IllegalStateException If the Relocator is not on the stack.
     * @throws IllegalStateException If the speed was already set.
     */
    public abstract void setSpeed(double speed);

    /**
     * Sets a callback object that receives events during the actual move.
     * This is optional and is not required to be set.
     *
     * @param callback The callback to register for this move.
     * @throws IllegalStateException If the Relocator is not on the stack.
     * @throws IllegalStateException If a callback was already set.
     */
    public abstract void setCallback(IMovementCallback callback);

    /**
     * Following methods are used to add blocks to the queue.
     *
     * @throws IllegalStateException If the Relocator is not on the stack.
     */
    public abstract void addBlock(BlockPos bc);

    public abstract void addBlocks(Set<BlockPos> blocks);

    /**
     * Used to execute the queued movement. Before calling, the world, direction,
     * and queue of blocks must be set.
     *
     * @return True if the movement was successfully started.
     * @throws IllegalStateException If the Relocator is not on the stack.
     * @throws IllegalStateException If any of the required parameters are not set
     *                               or are invalid.
     */
    public abstract boolean execute();
}