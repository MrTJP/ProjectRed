package mrtjp.projectred.transmission;

import codechicken.lib.vec.BlockCoord;

public class RedAlloyWirePart extends RedwirePart {
    public RedAlloyWirePart(int side) {
        super(side);
    }

    @Override
    public int getColour() {
        return ((strength&0xFF)/2 + 60) << 24 | 0xFF;
    }
    
    @Override
    public EnumWire getWireType() {
        return EnumWire.RED_ALLOY;
    }
    
    @Override
    public String getType() {
        return "pr_redwire";
    }
    
    @Override
    public int calculateUndersideStrength() {
        BlockCoord pos = new BlockCoord(getTile()).offset(side);
        return world().getIndirectPowerLevelTo(pos.x, pos.y, pos.z, side^1)*17;
    }
    
    @Override
    public void propogateOther() {
        for(int s = 0; s < 6; s++)
            WirePropogator.neighborChanges.add(new BlockCoord(getTile()).offset(side).offset(s));
    }
    
    @Override
    public int redstoneConductionMap() {
        return 0x1F;
    }
}
