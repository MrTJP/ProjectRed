package mrtjp.projectred.core.part;

import javax.annotation.Nullable;

public interface IPropagationPart {

    /**
     * Recalculates the signal of this wire and calls the appropriate
     * propagation methods in WirePropagator. DO NOT CALL THIS YOURSELF. Use
     * WirePropagator.propagateTo
     *
     * @param prev The part which called this propagation (should be connected)
     *             may be null.
     * @param mode One of RISING, DROPPING, FORCE and FORCED specified above
     */
    void updateAndPropagate(@Nullable IPropagationPart prev, int mode);

    /**
     * Called at the end of a propagation run for partChanged events. Marks the
     * end of a state change for this part.
     */
    void onSignalUpdate();
}
