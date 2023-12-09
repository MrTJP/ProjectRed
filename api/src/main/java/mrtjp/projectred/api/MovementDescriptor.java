package mrtjp.projectred.api;

import net.minecraft.world.level.Level;

import java.util.Set;

/**
 * Description returned from attempting to execute a movement via {@link IExpansionAPI#beginMove(Level, int, double, Set)}. Can
 * be used to check status and keep track of its progress. It is recommended that this object is discarded once the movement is complete,
 * as it may prevent internal movement references from being cleaned up.
 */
public interface MovementDescriptor {

    enum MovementStatus {
        /**
         * Failed to begin movement (collision, unmovable block in structure, etc)
         */
        FAILED,
        /**
         * Movement in progress
         */
        MOVING,
        /**
         * Movement finished successfully
         */
        FINISHED,
        /**
         * Movement cancelled before completion (chunk was unloaded, etc)
         */
        CANCELLED,
        /**
         * Unknown status
         */
        UNKNOWN
    }

    /**
     * Gets the current status of the movement.
     */
    MovementStatus getStatus();

    /**
     * @return True if the structure is moving.
     */
    boolean isMoving();

    /**
     * @return The progress of the movement scaled between 0 and 1.
     */
    double getProgress();

    /**
     * @return The size of the moving structure.
     */
    int getSize();
}