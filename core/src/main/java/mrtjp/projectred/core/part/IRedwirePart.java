package mrtjp.projectred.core.part;

public interface IRedwirePart extends IRedwireEmitter {

    /**
     * @param side The side of this part to test for wire connection. For face
     *             parts, a rotation, or -1 for center (up). For center parts, a
     *             direction index.
     * @return true if the specified side of this block is connected to, for
     *         example, a 'wire' where signal should decrease by one.
     */
    boolean diminishOnSide(int side);
}
