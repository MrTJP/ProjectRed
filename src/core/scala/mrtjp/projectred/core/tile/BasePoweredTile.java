package mrtjp.projectred.core.tile;

import mrtjp.projectred.core.PowerConductor;
import mrtjp.projectred.core.power.ConductorCache;
import net.minecraft.tileentity.ITickableTileEntity;
import net.minecraft.tileentity.TileEntityType;
import net.minecraft.util.math.BlockPos;

public abstract class BasePoweredTile extends BaseConnectableTile implements IPoweredTile, ITickableTileEntity {

    /**
     * Full-faced tile can connect to a total of 30 different conductors:
     *  - 6 faces
     *  - 5 connections per face (4 on each edge plus one in center)
     */
    protected ConductorCache conductorCache = new ConductorCache(this::getConductor, 30);

    public BasePoweredTile(TileEntityType<?> type) {
        super(type);
    }

    protected void invalidateConductorCache() {
        conductorCache.invalidateCache();
    }

    @Override
    public void onMaskChanged() {
        super.onMaskChanged();
        conductorCache.invalidateCache();
    }

    @Override
    public void onNeighborBlockChanged(BlockPos neighborPos) {
        super.onNeighborBlockChanged(neighborPos);
        conductorCache.invalidateCache();
    }

    @Override
    public PowerConductor conductorOut(int id) {
        return conductorCache.getExternalConductor(id);
    }

    private PowerConductor getConductor(int id) {
        if (id < 24) {
            int s = id / 4;
            int edgeRot = id % 4;
            return IPoweredTile.getExternalConductorForFaceConn(this, s, edgeRot);
        }

        if (id < 30) {
            int s = id - 24;
            return IPoweredTile.getExternalConductorForCenterConn(this, s);
        }

        return null;
    }
}
