package mrtjp.projectred.transmission.part;

import mrtjp.projectred.core.IPowerConnectable;
import mrtjp.projectred.core.JPowerConductor;

import java.util.Collection;

public class LowLoadPowerConductor extends JPowerConductor {

    public LowLoadPowerConductor(IPowerConnectable parent, Collection<Integer> ids) {
        super(parent, ids);
    }

    @Override
    public double capacitance() {
        return 8.0D;
    }

    @Override
    public double resistance() {
        return 0.01D;
    }

    @Override
    public double scaleOfInductance() {
        return 0.07D;
    }

    @Override
    public double scaleOfParallelFlow() {
        return 0.5D;
    }
}
