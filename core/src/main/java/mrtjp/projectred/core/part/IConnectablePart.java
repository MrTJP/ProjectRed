package mrtjp.projectred.core.part;

import mrtjp.projectred.api.IConnectable;

/**
 * Sets of defs that are common in all subtypes.
 * dir is rotation for face, absDir for center implementations
 */
public interface IConnectablePart extends IConnectable {

    int getConnMap();

    void setConnMap(int map);

    /**
     * Recalculates connections that can be made to other parts outside this
     * space
     * <p>
     * When using this method be sure to call TConnectableCommons#onMaskChanged()
     * as needed as it is not called for you.
     *
     * @return true if external connections should be recalculated
     */
    boolean updateOpenConns();

    /**
     * Recalculates connections to blocks outside this space
     * <p>
     * When using this method be sure to call TConnectableCommons#onMaskChanged()
     * as needed as it is not called for you.
     *
     * @return true if a new connection was added or one was removed
     */
    boolean updateExternalConns();

    /**
     * Recalculates connections to other parts within this space
     * <p>
     * When using this method be sure to call TConnectableCommons#onMaskChanged()
     * as needed as it is not called for you.
     *
     * @return true if a new connection was added or one was removed
     */
    boolean updateInternalConns();

    /**
     * Updates internal connections first, then updates open conns.
     * If open conns change, then external conns are updated.
     * <p>
     * Use this when an internal part change occurs.
     */
    default void updateOutward() {
        // Update internal conns first
        boolean internal = updateInternalConns();
        // If open conns changed, update external conns as well
        boolean external = updateOpenConns() && updateExternalConns();
        maskChangeEvent(internal, external);
    }

    /**
     * Full update for internal and external connections.
     * <p>
     * Use when part is added, etc.
     */
    default void updateInsideAndOutside() {
        updateOpenConns();
        boolean internal = updateInternalConns();
        boolean external = updateExternalConns();
        maskChangeEvent(internal, external);
    }

    /**
     * Update external connections only.
     * <p>
     * Use when only neighbors have changed and not any other part
     */
    default void updateOutside() {
        boolean external = updateExternalConns();
        maskChangeEvent(false, external);
    }

    void maskChangeEvent(boolean internalChange, boolean externalChange);

    void notifyAllExternals();
}
