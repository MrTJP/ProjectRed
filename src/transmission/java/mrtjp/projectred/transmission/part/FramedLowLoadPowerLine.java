package mrtjp.projectred.transmission.part;

import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.ILowLoadPowerLine;
import mrtjp.projectred.core.IPowerConnectable;
import mrtjp.projectred.core.PowerConductor;
import mrtjp.projectred.transmission.WireType;

import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class FramedLowLoadPowerLine extends FramedPowerWire implements ILowLoadPowerLine {

    private final PowerConductor conductor = new LowLoadPowerConductor(this,
            IntStream.range(0, 6)
                    .boxed()
                    .collect(Collectors.toList()));

    public FramedLowLoadPowerLine(WireType wireType) {
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
