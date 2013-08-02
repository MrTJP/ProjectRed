package mrtjp.projectred.transmission;

import mrtjp.projectred.interfaces.wiring.IBareRedstoneWire;

public class TilePlainRedAlloy extends RedwirePart implements IBareRedstoneWire {
	
	@Override
	public int getVisualWireColour() {
		return ((getRedstoneSignalStrength()/2) + 60) << 16;
	}

	{
		syncSignalStrength = true;
		connectToBlockBelow = true;
	}
}
