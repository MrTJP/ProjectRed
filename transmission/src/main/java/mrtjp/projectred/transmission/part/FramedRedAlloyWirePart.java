package mrtjp.projectred.transmission.part;

import mrtjp.projectred.transmission.WireType;

public class FramedRedAlloyWirePart extends FramedRedwirePart {

    public FramedRedAlloyWirePart(WireType wireType) {
        super(wireType);
    }

    @Override
    public int getRenderHue() {
        return (getSignal() & 0xFF) / 2 + 60 << 24 | 0xFF;
    }
}
