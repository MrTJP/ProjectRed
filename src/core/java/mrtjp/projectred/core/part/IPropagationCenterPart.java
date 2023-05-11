package mrtjp.projectred.core.part;

import codechicken.multipart.api.part.TMultiPart;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.RedstonePropagator;

import static mrtjp.projectred.core.RedstonePropagator.FORCED;

public interface IPropagationCenterPart extends IPropagationHooks, IConnectableCenterPart {

    default void propagateOther(int mode) { }

    default void propagateForward(IPropagationPart prev, int mode) {
        if (mode != FORCED) RedstonePropagator.addPartChange((TMultiPart) this);

        for (int s = 0; s < 6; s++) {
            if ((getPropagationMask() & 1 << s) != 0) {
                if (maskConnectsIn(s)) {
                    propagateTo(getInternal(s), prev, mode);

                } else if (maskConnectsOut(s)) {
                    if (!propagateTo(getStraight(s), prev, mode)) {
                        RedstonePropagator.addNeighborChange(getLevel(), posOfStraight(s));
                    }
                }
            }
        }

        propagateOther(mode);
    }

    @Override
    default void propagateBackward(IPropagationPart prev, int mode) {
        if (shouldPropagate(prev, mode)) {
            RedstonePropagator.propagateTo(prev, this, mode);
        }
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

    /**
     * Provides a 4-bit rotation mask marking sides that will forward-propagate.
     */
    default int getPropagationMask() {
        return 0x3F;
    }
}
