package mrtjp.projectred.core.part;

/**
 * Common callbacks used by propagation models for both face and center parts.
 */
public interface IPropagationHooks extends IPropagationPart {

    /**
     * Perform forward propagation. That is, for every part connected to this part, call
     * RestonePropagator.propagateTo()
     * @param prev Previous part
     * @param mode Propagation mode
     */
    void propagateForward(IPropagationPart prev, int mode);

    /**
     * Perform back propagation to the previous part only
     * @param prev Previous part
     * @param mode Propagation mode
     */
    void propagateBackward(IPropagationPart prev, int mode);

    /**
     * Should be called by above forward/backwards methods as a final check to see if propagation
     * should continue to the given part.
     * @param to Part to propagate to
     * @param mode Propagation mode
     * @return True if propagation should continue
     */
    default boolean shouldPropagate(IPropagationPart to, int mode) {
        return true;
    }
}
