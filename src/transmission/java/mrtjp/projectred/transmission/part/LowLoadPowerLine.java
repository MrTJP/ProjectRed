package mrtjp.projectred.transmission.part;

import codechicken.multipart.api.part.ITickablePart;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.ILowLoadPowerLine;
import mrtjp.projectred.core.IPowerConnectable;
import mrtjp.projectred.core.PowerConductor;
import mrtjp.projectred.transmission.WireType;

import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class LowLoadPowerLine extends FacePowerWire implements ILowLoadPowerLine, ITickablePart {

    private final PowerConductor conductor = new LowLoadPowerConductor(this,
            IntStream.range(0, 5)
                    .boxed()
                    .collect(Collectors.toList()));

    public LowLoadPowerLine(WireType wireType) {
        super(wireType);
    }

    @Override
    public boolean canConnectPart(IConnectable part, int dir) {
        return part instanceof IPowerConnectable;
    }

    @Override
    public PowerConductor conductor(int dir) {
        return conductor;
    }

    @Override
    public void tick() {
        if (!world().isClientSide) {
            conductor.update();
        }
    }
}
