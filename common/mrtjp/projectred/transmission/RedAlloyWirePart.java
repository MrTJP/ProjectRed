package mrtjp.projectred.transmission;

import mrtjp.projectred.interfaces.wiring.IBareRedstoneWire;

public class RedAlloyWirePart extends RedwirePart implements IBareRedstoneWire {
	
	public RedAlloyWirePart(EnumWire type, boolean isJacketedWire, int onside) {
		super(type, isJacketedWire, onside);
	}

	@Override
	public int getVisualWireColour() {
		return ((getRedstoneSignalStrength()/2) + 60) << 16;
	}

	{
		syncSignalStrength = true;
		connectToBlockBelow = true;
	}
}
