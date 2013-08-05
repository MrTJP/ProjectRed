package mrtjp.projectred.expansion;

import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.integration.IntegrationSPH;
import codechicken.lib.packet.PacketCustom;

public class ExpansionClientProxy extends ExpansionProxy {

	@Override
	public void init() {
		PacketCustom.assignHandler(Configurator.expansionPacketChannel, 0, 32, new ExpansionCPH());
	}
}
