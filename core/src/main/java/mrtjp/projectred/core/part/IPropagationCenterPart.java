package mrtjp.projectred.core.part;

import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.RedstonePropagator;

import javax.annotation.Nullable;

import static mrtjp.projectred.core.RedstonePropagator.FORCED;

public interface IPropagationCenterPart extends IPropagationHooks, IConnectableCenterPart {

    default void propagateOther(int mode) { }

    default void propagateForward(@Nullable IPropagationPart prev, int mode) {
        if (mode != FORCED) RedstonePropagator.addPartChange(this);

        for (int s = 0; s < 6; s++) {
            if ((getPropagationMask() & 1 << s) != 0) {
                if (maskConnectsIn(s)) {
                    propagateTo(getInternal(s), prev, mode);

                } else if (maskConnectsOut(s)) {
                    if (!propagateTo(getStraight(s), prev, mode)) {
                        RedstonePropagator.addNeighborChange(level(), pos(), posOfStraight(s));
                    }
                }
            }
        }

        propagateOther(mode);
    }

    @Override
    default void propagateBackward(@Nullable IPropagationPart prev, int mode) {
        if (prev != null && shouldPropagate(prev, mode)) {
            RedstonePropagator.propagateTo(prev, this, mode);
        }
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

    /**
     * Provides a 4-bit rotation mask marking sides that will forward-propagate.
     */
    default int getPropagationMask() {
        return 0x3F;
    }
}
