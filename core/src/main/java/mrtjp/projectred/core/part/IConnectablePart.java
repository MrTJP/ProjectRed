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
     * Start update chain starting from an internal change outward
     * <p>
     * When using this method be sure to call TConnectableCommons#onMaskChanged()
     * as needed as it is not called for you.
     *
     * @return true if a new connection was added or one was removed
     */
    default boolean updateOutward() {
        boolean changed = updateInternalConns();
        if (updateOpenConns()) changed |= updateExternalConns();
        return changed;
    }

    /**
     * Start update chain starting from an external change inward
     * <p>
     * When using this method be sure to call TConnectableCommons#onMaskChanged()
     * as needed as it is not called for you.
     *
     * @return true if a new connection was added or one was removed
     */
    default boolean updateInward() {
        updateOpenConns();
        boolean changed = updateInternalConns();
        changed |= updateExternalConns();
        return changed;
    }

    void notifyAllExternals();

    void onMaskChanged();
}
