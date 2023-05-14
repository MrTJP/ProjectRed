package mrtjp.projectred.core.power;

import java.util.List;

public interface IPowerConductorSource {

    /**
     * Getter for world time. Used for electrical simulation
     * @return World game time
     */
    long getTime();

    /**
     * Returns all connected conductors
     */
    List<PowerConductor> getConnectedConductors();
}
