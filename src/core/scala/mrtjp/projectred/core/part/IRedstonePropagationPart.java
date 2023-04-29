package mrtjp.projectred.core.part;

import mrtjp.projectred.core.RedstonePropagator;

import static mrtjp.projectred.core.RedstonePropagator.*;

/**
 * Provides default propagation implementation for {@link IPropagationPart} for standard redstone
 * propagation rules.
 */
public interface IRedstonePropagationPart extends IPropagationPart, IPropagationHooks {

    //region Trait variables
    int getSignal();
    void setSignal(int signal);
    //endregion

    /**
     * Actively calculates the current Redstone signal this part is receiving.
     * @return The calculated redstone signal
     */
    int calculateSignal();

    @Override
    default void updateAndPropagate(IPropagationPart prev, int mode) {
        if (mode == DROPPING && getSignal() == 0) return;

        int oldSignal = getSignal();
        int newSignal = calculateSignal();

        if (newSignal < oldSignal) {
            if (newSignal > 0) {
                RedstonePropagator.propagateAnalogDrop(this);
            }
            setSignal(0);
            propagateForward(prev, DROPPING);

        } else if (newSignal > oldSignal) {
            setSignal(newSignal);
            if (mode == DROPPING) {
                propagateForward(null, RISING);
            } else {
                propagateForward(prev, RISING);
            }

        } else if (mode == FORCE) {
            propagateForward(prev, FORCED);

        } else if (mode == DROPPING) {
            // Dropping propagation, but no signal change. Propagate rising back
            propagateBackward(prev, RISING);
        }
    }
}
