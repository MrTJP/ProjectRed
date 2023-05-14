package mrtjp.projectred.core.power;

public interface IPowerConnectable {
    /**
     * Getter for the local conductor
     * @param dir Side of the required conductor, this
     *             is only used if the tile has multiple
     *             linked conductors (such as voltage transformers).
     *             Rotation for face parts, absDir else.
     * @return The local conductor managed by this object.
     */
    PowerConductor getConductor(int dir);
}
