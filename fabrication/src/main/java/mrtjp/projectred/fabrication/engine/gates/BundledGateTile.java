package mrtjp.projectred.fabrication.engine.gates;

import mrtjp.projectred.fabrication.engine.IBundledConnectableICTile;
import mrtjp.projectred.fabrication.engine.IConnectableICTile;

public class BundledGateTile extends RedstoneGateTile implements IBundledConnectableICTile {

    public BundledGateTile(ICGateTileType gateType) {
        super(gateType);
    }

    //region BundledGateTile override points
    @Override
    protected boolean canGateConnectTo(IConnectableICTile target, int r) {
        if (target instanceof IBundledConnectableICTile) {
            return canConnectBundled(r);
        }
        return super.canGateConnectTo(target, r);
    }

    protected boolean canConnectBundled(int r) {
        return canInputBundled(r) || canOutputBundled(r);
    }

    protected boolean canOutputBundled(int r) {
        return (bundledOutputMask() & 1 << r) != 0;
    }

    protected boolean canInputBundled(int r) {
        return (bundledInputMask() & 1 << r) != 0;
    }

    protected int bundledOutputMask() {
        return 0;
    }

    protected int bundledInputMask() {
        return 0;
    }
    //endregion
}
