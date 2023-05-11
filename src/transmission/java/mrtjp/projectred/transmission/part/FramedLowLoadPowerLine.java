package mrtjp.projectred.transmission.part;

import codechicken.multipart.api.part.ITickablePart;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.power.ILowLoadPowerLine;
import mrtjp.projectred.core.power.IPowerConnectable;
import mrtjp.projectred.core.power.PowerConductor;
import mrtjp.projectred.transmission.WireType;

public class FramedLowLoadPowerLine extends FramedPowerWire implements ILowLoadPowerLine, ITickablePart {

    private final PowerConductor conductor = new PowerConductor(this, 0.01, 16);

    public FramedLowLoadPowerLine(WireType wireType) {
        super(wireType);
    }

    @Override
    public boolean canConnectPart(IConnectable part, int dir) {
        return part instanceof IPowerConnectable;
    }

    @Override
    public PowerConductor getConductor(int dir) {
        return conductor;
    }

    @Override
    public void tick() {
        if (!world().isClientSide) {
            conductor.tick();
        }
    }
}
