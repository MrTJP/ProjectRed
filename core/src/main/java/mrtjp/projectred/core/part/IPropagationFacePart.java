package mrtjp.projectred.core.part;

import codechicken.multipart.api.part.TMultiPart;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.RedstonePropagator;

public interface IPropagationFacePart extends IPropagationHooks, IConnectableFacePart {

    default void propagateOther(int mode) { }

    default void propagateForward(IPropagationPart prev, int mode) {
        if (mode != RedstonePropagator.FORCED) {
            RedstonePropagator.addPartChange((TMultiPart) this);
        }

        for (int r = 0; r < 4; r++) {
            if ((getPropagationMask() & 1 << r) != 0) {
                if (maskConnectsInside(r)) {
                    propagateTo(getInternal(r), prev, mode);
                } else if (maskConnectsStraight(r)) {
                    if (!propagateTo(getStraight(r), prev, mode)) {
                        RedstonePropagator.addNeighborChange(level(), posOfStraight(r));
                    }
                } else if (maskConnectsCorner(r)) {
                    if (!propagateTo(getCorner(r), prev, mode)) {
                        RedstonePropagator.addNeighborChange(level(), posOfCorner(r));
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
    default void propagateBackward(IPropagationPart prev, int mode) {
        if (shouldPropagate(prev, mode)) {
            RedstonePropagator.propagateTo(prev, this, mode);
        }
    }

    /**
     * Provides a 4-bit rotation mask marking sides that will forward-propagate.
     */
    default int getPropagationMask() {
        return 0xF;
    }

    default boolean propagateTo(IConnectable to, IPropagationPart prev, int mode) {
        if (to != null) {
            if (to == prev) return false;
            if (to instanceof IPropagationPart) {
                IPropagationPart part = (IPropagationPart) to;
                if (shouldPropagate(part, mode)) {
                    RedstonePropagator.propagateTo(part, this, mode);
                    return true;
                }
            }
        }
        return false;
    }
}
