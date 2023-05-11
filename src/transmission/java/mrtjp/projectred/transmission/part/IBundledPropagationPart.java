package mrtjp.projectred.transmission.part;

import mrtjp.projectred.core.RedstonePropagator;
import mrtjp.projectred.core.part.IPropagationHooks;
import mrtjp.projectred.core.part.IPropagationPart;

import static mrtjp.projectred.core.BundledSignalsLib.*;
import static mrtjp.projectred.core.RedstonePropagator.*;

public interface IBundledPropagationPart extends IPropagationPart, IPropagationHooks {

    //region Trait variables
    byte[] getSignal();
    void setSignal(byte[] signal);
    int getColorMask();
    void setColorMask(int mask);
    //endregion

    /**
     * Actively calculates current bundled signal based on neighbor emissions
     * @return new bundled signals
     */
    byte[] calculateSignal();

    @Override
    default void updateAndPropagate(IPropagationPart from, int mode) {
        int mask = getColorMaskFrom(from, mode);
        if (mode == DROPPING && isSignalZero(getSignal(), mask)) return;

        byte[] newSignal = calculateSignal();
        applyChangeMask(getSignal(), newSignal, mask);

        setColorMask(mask);

        if (dropSignalsLessThan(getSignal(), newSignal)) {
            if (!isSignalZero(newSignal, mask)) {
                RedstonePropagator.propagateAnalogDrop(this);
            }
            propagateForward(from, DROPPING);
        } else if (!signalsEqual(getSignal(), newSignal)) {
            setSignal(newSignal);
            if (mode == DROPPING) {
                propagateForward(null, RISING);
            } else {
                propagateForward(from, RISING);
            }
        } else if (mode == FORCE) {
            propagateForward(from, FORCED);
        }  else if (mode == DROPPING) {
            propagateBackward(from, RISING);
        }

        setColorMask(0xFFFF);
    }

    default int getColorMaskFrom(IPropagationPart from, int mode) {
        if (from instanceof IInsulatedRedwirePart) {
            return 1 << ((IInsulatedRedwirePart) from).getInsulatedColour();
        }

        if (from instanceof IBundledCablePart && mode == DROPPING) {
            int m = 0;
            byte[] signalIn = ((IBundledCablePart) from).getBundledSignal();
            for (int i = 0; i < 16; i++) {
                if (signalIn[i] == 0) {
                    m |= 1 << i;
                }
            }
            return m;
        }

        if (from instanceof IBundledCablePart && mode == RISING) {
            int m = 0;
            byte[] signalIn = ((IBundledCablePart) from).getBundledSignal();
            for (int i = 0; i < 16; i++) {
                if ((signalIn[i] & 0xFF) > (getSignal()[i] & 0xFF)) {
                    m |= 1 << i;
                }
            }
            return m;
        }

        return 0xFFFF;
    }

    @Override
    default boolean shouldPropagate(IPropagationPart to, int mode) {
        // Don't propagate to insulated colors if the color is masked
        if (to instanceof IInsulatedRedwirePart) {
            int insColour = ((IInsulatedRedwirePart) to).getInsulatedColour();
            return (getColorMask() & 1 << insColour) != 0;
        }

        return true;
    }
}
