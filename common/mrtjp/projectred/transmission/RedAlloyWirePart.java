package mrtjp.projectred.transmission;


public class RedAlloyWirePart extends RedwirePart {
    
    
    public RedAlloyWirePart(EnumWire type, boolean isJacketedWire, int onside) {
        super(type, isJacketedWire, onside);
        syncSignalStrength = true;
        connectToBlockBelow = true;
    }

    @Override
    public int getVisualWireColour() {
        return ((getRedstoneSignalStrength()/2) + 60) << 16;
    }
}
