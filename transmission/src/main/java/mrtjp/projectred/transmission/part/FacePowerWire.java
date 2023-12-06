package mrtjp.projectred.transmission.part;

import codechicken.lib.vec.Rotation;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.FaceLookup;
import mrtjp.projectred.core.power.IPowerConductorSource;
import mrtjp.projectred.core.power.IPowerConnectable;
import mrtjp.projectred.core.power.PowerConductor;
import mrtjp.projectred.transmission.WireType;
import net.minecraft.core.BlockPos;

import javax.annotation.Nullable;
import java.util.LinkedList;
import java.util.List;

public abstract class FacePowerWire extends BaseFaceWirePart implements IPowerConnectable, IPowerConductorSource {

    private final List<PowerConductor> connectedConductors = new LinkedList<>();
    private boolean cacheInvalid = true;

    public FacePowerWire(WireType wireType) {
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

    private void recacheConductors() {
        connectedConductors.clear();

        FaceLookup lookup;
        for (int r = 0; r < 4; r++) {
            if (maskConnectsCorner(r)) {
                lookup = FaceLookup.lookupCorner(level(), pos(), getSide(), r);
            } else if (maskConnectsStraight(r)) {
                lookup = FaceLookup.lookupStraight(level(), pos(), getSide(), r);
            } else if (maskConnectsInside(r)) {
                lookup = FaceLookup.lookupInsideFace(level(), pos(), getSide(), r);
            } else {
                continue;
            }

            PowerConductor c = retrieveConductor(lookup);
            if (c != null) connectedConductors.add(c);
        }

        if (maskConnectsCenter()) {
            lookup = FaceLookup.lookupInsideCenter(level(), pos(), getSide());
            PowerConductor c = retrieveConductor(lookup);
            if (c != null) connectedConductors.add(c);
        }
    }

    private @Nullable PowerConductor retrieveConductor(FaceLookup lookup) {

        if (lookup.part instanceof IPowerConnectable) {
            return ((IPowerConnectable) lookup.part).getConductor(lookup.otherRotation);
        }

        if (lookup.tile instanceof IPowerConnectable) {
            return ((IPowerConnectable) lookup.tile).getConductor(Rotation.rotateSide(lookup.otherSide, lookup.otherRotation));
        }

        return null;
    }

    //region Connections
    @Override
    public boolean discoverCornerOverride(int absDir) {
        int r = absoluteRot(absDir);
        FaceLookup lookup = FaceLookup.lookupCorner(level(), pos(), getSide(), r);
        if (lookup.tile instanceof IConnectable) {
            return ((IConnectable) lookup.tile).connectCorner(this, getSide() ^ 1, Rotation.rotationTo(getSide(), absDir ^ 1));
        }
        return false;
    }

    @Override
    public boolean discoverStraightOverride(int absDir) {
        int r = absoluteRot(absDir);
        FaceLookup lookup = FaceLookup.lookupStraight(level(), pos(), getSide(), r);
        if (lookup.tile instanceof IConnectable) {
            return ((IConnectable) lookup.tile).connectStraight(this, absDir ^ 1, Rotation.rotationTo(absDir, getSide()));
        }
        return false;
    }
    //endregion
}
