package mrtjp.projectred.core.part;

/**
 * Implemented by parts that emit a full-strength red alloy signal.
 */
public interface IRedwireEmitter {

    /**
     * For face parts, dir is a rotation. For center parts, it is a forge
     * direction.
     *
     * @return Signal strength from 0 to 255.
     */
    int getRedwireSignal(int dir);
}
