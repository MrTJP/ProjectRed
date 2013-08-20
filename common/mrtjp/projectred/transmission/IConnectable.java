package mrtjp.projectred.transmission;


/**
 * Interface implemented by face parts to connect to various types of wires.
 */
public interface IConnectable {
    /**
     * Called to check whether a wire/logic part can connect to this.
     * If a part returns true it is expected to immediately reflect the fact that it is now connected to wire.
     * 
     * @param part The part asking for connection.
     * @param r The clockwise rotation about the attached face to 
     * @return True to allow the wire connection.
     */
    public boolean connectStraight(IConnectable part, int r);

    /**
     * Connect for internals
     */
    public boolean connectInternal(IConnectable part, int r);

    /**
     * Connect for corners
     */
    public boolean connectCorner(IConnectable part, int r);
}
