package mrtjp.projectred.transmission.part;

import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.CenterLookup;
import mrtjp.projectred.core.power.IPowerConductorSource;
import mrtjp.projectred.core.power.IPowerConnectable;
import mrtjp.projectred.core.power.PowerConductor;
import mrtjp.projectred.transmission.WireType;
import net.minecraft.core.BlockPos;

import javax.annotation.Nullable;
import java.util.LinkedList;
import java.util.List;

public abstract class FramedPowerWire extends BaseCenterWirePart implements IPowerConnectable, IPowerConductorSource {

    private final List<PowerConductor> connectedConductors = new LinkedList<>();
    private boolean cacheInvalid = true;

    public FramedPowerWire(WireType wireType) {
        super(wireType);
    }

    @Override
    public long getTime() {
        return level().getGameTime();
    }

    @Override
    public List<PowerConductor> getConnectedConductors() {
        if (cacheInvalid) {
            recacheConductors();
            cacheInvalid = false;
        }
        return connectedConductors;
    }

    private void recacheConductors() {
        connectedConductors.clear();

        for (int s = 0; s < 6; s++) {
            CenterLookup lookup;
            if (maskConnectsIn(s)) {
                lookup = CenterLookup.lookupInsideFace(level(), pos(), s);
            } else if (maskConnectsOut(s)) {
                lookup = CenterLookup.lookupStraightCenter(level(), pos(), s);
            } else {
                continue;
            }

            PowerConductor c = retrieveConductor(lookup);
            if (c != null) connectedConductors.add(c);
        }
    }

    private @Nullable PowerConductor retrieveConductor(CenterLookup lookup) {
        if (lookup.part instanceof IPowerConnectable) {
            return ((IPowerConnectable) lookup.part).getConductor(lookup.otherDirection);
        }

        if (lookup.tile instanceof IPowerConnectable) {
            return ((IPowerConnectable) lookup.tile).getConductor(lookup.otherDirection);
        }

        return null;
    }

    @Override
    public void onMaskChanged() {
        super.onMaskChanged();
        cacheInvalid = true;
    }

    @Override
    public void onNeighborBlockChanged(BlockPos from) {
        super.onNeighborBlockChanged(from);
        cacheInvalid = true;
    }

    //region Connections
    @Override
    public boolean discoverStraightOverride(int absDir) {
        CenterLookup lookup = CenterLookup.lookupStraightCenter(level(), pos(), absDir);
        if (lookup.tile instanceof IConnectable) {
            return ((IConnectable) lookup.tile).connectStraight(this, lookup.otherDirection, -1);
        }
        return false;
    }
    //endregion
}
