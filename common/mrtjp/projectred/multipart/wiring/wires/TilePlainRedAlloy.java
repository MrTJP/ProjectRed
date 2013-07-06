package mrtjp.projectred.multipart.wiring.wires;

import mrtjp.projectred.interfaces.wiring.IBareRedstoneWire;

public class TilePlainRedAlloy extends TileRedAlloy implements IBareRedstoneWire {

	@Override
	public int getVisualWireColour() {
		return (getRedstoneSignalStrength() / 2 + 127) << 16;
	}

	{
		syncSignalStrength = true;
		connectToBlockBelow = true;
	}
}
