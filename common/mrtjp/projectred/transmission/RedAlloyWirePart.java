package mrtjp.projectred.transmission;

import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Rotation;

public class RedAlloyWirePart extends RedwirePart {
    public RedAlloyWirePart(int side) {
        super(side);
    }

    @Override
    public String getType() {
        return "pr_redwire";
    }

    @Override
    public EnumWire getWireType() {
        return EnumWire.RED_ALLOY;
    }
    
    @Override
    public int getColour() {
        return ((signal&0xFF)/2 + 60) << 24 | 0xFF;
    }

    @Override
    public int strongPowerLevel(int side) {
        if(side == this.side)
            return rsLevel();
        
        return 0;
    }
    
    @Override
    public int weakPowerLevel(int side) {
        if(this.side == side || this.side == (side^1) ||
                maskConnectsToOutside(Rotation.rotationTo(this.side, side)))
            return super.weakPowerLevel(side);
        
        return 0;
    }

    @Override
    public int redstoneConductionMap() {
        return 0x1F;
    }

    @Override
    public void propogateOther(int mode) {
        WirePropogator.addNeighborChange(new BlockCoord(getTile()).offset(side));
        WirePropogator.addNeighborChange(new BlockCoord(getTile()).offset(side^1));
        for(int r = 0; r < 4; r++)
            if(!maskConnects(r))
                WirePropogator.addNeighborChange(new BlockCoord(getTile()).offset(Rotation.rotateSide(side, r)));
        
        for(int s = 0; s < 6; s++)
            if(s != (side^1))
                WirePropogator.addNeighborChange(new BlockCoord(getTile()).offset(side).offset(s));
    }

    @Override
    public int calculateUndersideSignal() {
        BlockCoord pos = new BlockCoord(getTile()).offset(side);
        return world().getIndirectPowerLevelTo(pos.x, pos.y, pos.z, side)*17;
    }
}
