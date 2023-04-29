package mrtjp.projectred.transmission.part;

import codechicken.multipart.api.part.ITickablePart;
import mrtjp.projectred.core.IPowerConnectable;
import mrtjp.projectred.core.PowerConductor;
import mrtjp.projectred.core.power.ConductorCache;
import mrtjp.projectred.core.CenterLookup;
import mrtjp.projectred.transmission.WireType;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public abstract class FramedPowerWire extends BaseCenterWirePart implements IPowerConnectable, ITickablePart {

    private final ConductorCache cache = new ConductorCache(this::getConductor, 6);

    public FramedPowerWire(WireType wireType) {
        super(wireType);
    }

    private PowerConductor getConductor(int s) {
        CenterLookup lookup = null;

        if (s < 0 || s > 6) return null;

        if (maskConnectsOut(s)) {
            lookup = CenterLookup.lookupStraightCenter(world(), pos(), s);
        } else if (maskConnectsIn(s)) {
            lookup = CenterLookup.lookupInsideFace(world(), pos(), s);
        }

        if (lookup == null) return null;

        if (lookup.part instanceof IPowerConnectable) {
            return ((IPowerConnectable) lookup.part).conductor(lookup.otherDirection);
        }

        if (lookup.tile instanceof IPowerConnectable) {
            return ((IPowerConnectable) lookup.tile).conductor(lookup.otherDirection);
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
    public boolean discoverStraightOverride(int absDir) {
        CenterLookup lookup = CenterLookup.lookupStraightCenter(world(), pos(), absDir);
        if (lookup.tile instanceof IPowerConnectable) {
            return ((IPowerConnectable) lookup.tile).connectStraight(this, lookup.otherDirection, -1);
        }
        return false;
    }
    //endregion
}
