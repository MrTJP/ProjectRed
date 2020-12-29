/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.api;

/**
 * This is the interface of the object that is given to an {@link IMovementCallback} so
 * it may access movement information whenever it needs to during the movement.
 * <p>
 * Results of all methods is only valid during movement, undefined otherwise.  Use
 * isMoving() to check before using any values from this.
 */
public interface IMovementDescriptor {
    /**
     * @return True if the structure is moving.
     */
    boolean isMoving();

    /**
     * @return The progress of the movement scaled between 0 and 1.
     */
    double getProgress();

    /**
     * @return The size of the moving structure, zero if it is no longer moving.
     */
    int getSize();
}