package mrtjp.projectred.transmission.part;

import codechicken.lib.vec.Rotation;
import mrtjp.projectred.core.IPowerConnectable;
import mrtjp.projectred.core.PowerConductor;
import mrtjp.projectred.core.power.ConductorCache;
import mrtjp.projectred.core.FaceLookup;
import mrtjp.projectred.transmission.WireType;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public abstract class FacePowerWire extends BaseFaceWirePart implements IPowerConnectable {

    private final ConductorCache cache = new ConductorCache(this::getConductor, 5);

    public FacePowerWire(WireType wireType) {
        super(wireType);
    }

    private PowerConductor getConductor(int i) {
        FaceLookup lookup = null;

        if (i > 0 && i <= 3) {
            if (maskConnectsCorner(i)) {
                lookup = FaceLookup.lookupCorner(world(), pos(), getSide(), i);
            } else if (maskConnectsStraight(i)) {
                lookup = FaceLookup.lookupStraight(world(), pos(), getSide(), i);
            } else if (maskConnectsInside(i)) {
                lookup = FaceLookup.lookupInsideFace(world(), pos(), getSide(), i);
            } else {
                return null;
            }
        }

        if (i == 4 && maskConnectsCenter()) {
            lookup = FaceLookup.lookupInsideCenter(world(), pos(), getSide());
        }

        if (lookup == null) return null;

        if (lookup.part instanceof IPowerConnectable) {
            return ((IPowerConnectable) lookup.part).conductor(lookup.otherRotation);
        }

        if (lookup.tile instanceof IPowerConnectable) {
            return ((IPowerConnectable) lookup.tile).conductor(Rotation.rotateSide(lookup.otherSide, lookup.otherRotation));
        }

        return null;
    }

    @Override
    public PowerConductor conductorOut(int id) {
        return cache.getExternalConductor(id);
    }

    @Override
    public World connWorld() {
        return world();
    }

    @Override
    public void onMaskChanged() {
        super.onMaskChanged();
        cache.invalidateCache();
    }

    @Override
    public void onNeighborBlockChanged(BlockPos from) {
        super.onNeighborBlockChanged(from);
        cache.invalidateCache();
    }

    //region Connections
    @Override
    public boolean discoverCornerOverride(int absDir) {
        int r = absoluteRot(absDir);
        FaceLookup lookup = FaceLookup.lookupCorner(world(), pos(), getSide(), r);
        if (lookup.tile instanceof IPowerConnectable) {
            return ((IPowerConnectable) lookup.tile).connectStraight(this, getSide() ^ 1, Rotation.rotationTo(getSide(), absDir ^ 1));
        }
        return false;
    }

    @Override
    public boolean discoverStraightOverride(int absDir) {
        int r = absoluteRot(absDir);
        FaceLookup lookup = FaceLookup.lookupStraight(world(), pos(), getSide(), r);
        if (lookup.tile instanceof IPowerConnectable) {
            return ((IPowerConnectable) lookup.tile).connectStraight(this, absDir ^ 1, Rotation.rotationTo(absDir, getSide()));
        }
        return false;
    }
    //endregion
}
