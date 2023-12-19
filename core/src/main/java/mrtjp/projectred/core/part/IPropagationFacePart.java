package mrtjp.projectred.core.part;

import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.RedstonePropagator;

import javax.annotation.Nullable;

public interface IPropagationFacePart extends IPropagationHooks, IConnectableFacePart {

    default void propagateOther(int mode) { }

    default void propagateForward(@Nullable IPropagationPart prev, int mode) {
        if (mode != RedstonePropagator.FORCED) {
            RedstonePropagator.addPartChange(this);
        }

        for (int r = 0; r < 4; r++) {
            if ((getPropagationMask() & 1 << r) != 0) {
                if (maskConnectsInside(r)) {
                    propagateTo(getInternal(r), prev, mode);
                } else if (maskConnectsStraight(r)) {
                    if (!propagateTo(getStraight(r), prev, mode)) {
                        RedstonePropagator.addNeighborChange(level(), pos(), posOfStraight(r));
                    }
                } else if (maskConnectsCorner(r)) {
                    if (!propagateTo(getCorner(r), prev, mode)) {
                        RedstonePropagator.addNeighborChange(level(), posOfStraight(r), posOfCorner(r));
                    }
                }
            }
        }

        if (maskConnectsCenter()) {
            propagateTo(getCenter(), prev, mode);
        }

        propagateOther(mode);
    }

    @Override
    default void propagateBackward(@Nullable IPropagationPart prev, int mode) {
        if (prev != null && shouldPropagate(prev, mode)) {
            RedstonePropagator.propagateTo(prev, this, mode);
        }
    }

    /**
     * Provides a 4-bit rotation mask marking sides that will forward-propagate.
     */
    default int getPropagationMask() {
        return 0xF;
    }

    default boolean propagateTo(@Nullable IConnectable to, @Nullable IPropagationPart prev, int mode) {
        if (to != null) {
            if (to == prev) return false;
            if (to instanceof IPropagationPart part) {
                if (shouldPropagate(part, mode)) {
                    RedstonePropagator.propagateTo(part, this, mode);
                    return true;
                }
            }
        }
        return false;
    }
}
