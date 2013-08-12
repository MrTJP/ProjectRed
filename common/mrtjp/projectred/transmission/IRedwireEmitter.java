package mrtjp.projectred.transmission;

/**
 * Implemented by parts that emit a full-strength red alloy signal.
 */
public interface IRedwireEmitter {
    /**
     * For face parts, side is a rotation. For center parts, it is a forge direction.
     * 
     * @return Signal strength from 0 to 255.
     */
    public int getRedwireSignal(int side);
}
