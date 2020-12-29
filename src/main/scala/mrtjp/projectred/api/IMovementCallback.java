/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.api;

/**
 * Interface that can be used by things that wish to receive events for
 * a move that they executed.  This interface is passed into the {@link Relocator}
 * during movement construction.
 * <p>
 * For example, a motor may wish to know when the movement completes.  In which case,
 * the motor tile itself would implement this interface and pass itself into the
 * Relocator.
 * <p>
 * These methods are only called server-side.  Clients will not receive any of
 * these events, as this class is registered during the execution of a move, which
 * can only happen server-side.
 */
public interface IMovementCallback {
    /**
     * Called once move starts.  The descriptor contains methods that
     * describe the state of the current movement that this callback
     * was registered to.
     * <p>
     * This descriptor is properly weak-referenced, keep or discard it
     * at your own leasure.
     *
     * @param desc The movement descriptor for the current move.
     */
    void setDescriptor(IMovementDescriptor desc);

    /**
     * Called when the movement starts, when the animation starts playing.
     */
    void onMovementStarted();

    /**
     * Called when the movement completes, and all blocks have moved over.
     */
    void onMovementFinished();
}