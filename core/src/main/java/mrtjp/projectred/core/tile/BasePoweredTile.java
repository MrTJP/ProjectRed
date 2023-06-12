package mrtjp.projectred.core.tile;

import mrtjp.projectred.core.power.PowerConductor;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

import java.util.LinkedList;
import java.util.List;

public abstract class BasePoweredTile extends BaseConnectableTile implements IPoweredTile {

    private final List<PowerConductor> connectedConductors = new LinkedList<>();
    private boolean cacheInvalid = true;

    public BasePoweredTile(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
    }

    protected void invalidateConductorCache() {
        cacheInvalid = true;
    }

    @Override
    public void onMaskChanged() {
        super.onMaskChanged();
        cacheInvalid = true;
    }

    @Override
    public void onNeighborBlockChanged(BlockPos neighborPos) {
        super.onNeighborBlockChanged(neighborPos);
        cacheInvalid = true;
    }

    @Override
    public long getTime() {
        return level.getGameTime();
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

        // Edge connections
        for (int s = 0; s < 6; s++) {
            for (int r = 0; r < 4; r++) {
                PowerConductor c = IPoweredTile.getExternalConductorForFaceConn(this, s, r);
                if (c != null) connectedConductors.add(c);
            }
        }

        // Center connections
        for (int s = 0; s < 6; s++) {
            PowerConductor c = IPoweredTile.getExternalConductorForCenterConn(this, s);
            if (c != null) connectedConductors.add(c);
        }
    }

    public abstract int getConductorCharge();

    public abstract int getConductorFlow();

    public abstract boolean canConductorWork();
}
