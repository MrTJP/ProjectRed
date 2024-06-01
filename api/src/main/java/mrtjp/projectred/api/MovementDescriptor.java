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
         * Movement pending start. Default state when first created
         */
        PENDING_START,
        /**
         * Movement animation in progress while block remains in initial position
         */
        MOVING,
        /**
         * Movement animation has completed but still awaiting execution of movement.
         * Block is still in initial position.
         */
        PENDING_FINALIZATION,
        /**
         * Movement finished successfully and block is in final position
         */
        FINISHED,
        /**
         * Movement cancelled before completion (chunk was unloaded, etc)
         */
        CANCELLED,
        /**
         * Failed to begin movement (collision, unmovable block in structure, etc)
         */
        FAILED,
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