package mrtjp.projectred.integration;

import codechicken.multipart.INeighborTileChange;

public class InstancedRsGatePartT extends InstancedRsGatePart implements INeighborTileChange
{
    @Override
    public String getType() {
        return "pr_tgate";
    }
    
    @Override
    public void onNeighborTileChanged(int side, boolean weak) {
        ((INeighborTileChange)getLogic()).onNeighborTileChanged(side, weak);
    }
    
    @Override
    public boolean weakTileChanges() {
        return ((INeighborTileChange)getLogic()).weakTileChanges();
    }
}
