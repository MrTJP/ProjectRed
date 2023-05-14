package mrtjp.projectred.transmission.part;

import mrtjp.projectred.core.RedstonePropagator;
import mrtjp.projectred.transmission.WireType;
import net.minecraft.util.Direction;
import net.minecraft.util.math.BlockPos;

public class RedAlloyWirePart extends RedwirePart {

    public RedAlloyWirePart(WireType wireType) {
        super(wireType);
    }

    //region TMultiPart overrides
    @Override
    public void onRemoved() {
        super.onRemoved();
        if (!world().isClientSide) {
            tile().notifyNeighborChange(getSide());
        }
    }
    //endregion

    //region RedwirePart overrides
    @Override
    public int getRenderHue() {
        return (getSignal() & 0xFF) / 2 + 60 << 24 | 0xFF;
    }

    @Override
    protected boolean powerUnderside() {
        return true;
    }
    //endregion

    //region IRedstonePropagationFacePart overrides
    @Override
    public void propagateOther(int mode) {

        // Update block above and below
        RedstonePropagator.addNeighborChange(getLevel(), getPos().relative(Direction.values()[getSide()]));
        RedstonePropagator.addNeighborChange(getLevel(), getPos().relative(Direction.values()[getSide() ^ 1]));

        // Update all 4 rotational sides if they are not connected. They are excluded
        // because things that actually connect are expected to be part of the propagation anyway
        for (int r = 0; r < 4; r++) {
            if (!maskConnects(r)) {
                RedstonePropagator.addNeighborChange(getLevel(), posOfStraight(r));
            }
        }

        // Update all neighbors of the block below
        BlockPos posUnder = getPos().relative(Direction.values()[getSide()]);
        for (int s = 0; s < 6; s++) {
            if (s != (getSide() ^ 1)) {
                RedstonePropagator.addNeighborChange(getLevel(), posUnder.relative(Direction.values()[s]));
            }
        }
    }
    //endregion

    //region IRedstonePart overrides
    @Override
    public int strongPowerLevel(int side) {
        // Alloy wires strongly power underside
        return side == getSide() ? redstoneSignalLevel() : 0;
    }

    @Override
    public int redstoneConductionMap() {
        return 0x1F;
    }
    //endregion
}

