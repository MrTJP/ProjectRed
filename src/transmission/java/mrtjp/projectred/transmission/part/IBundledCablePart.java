package mrtjp.projectred.transmission.part;

import mrtjp.projectred.api.IBundledEmitter;

public interface IBundledCablePart extends IBundledEmitter {

    byte[] getBundledSignal();

    int getBundledColour();
}
