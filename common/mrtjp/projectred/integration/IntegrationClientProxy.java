package mrtjp.projectred.integration;

import mrtjp.projectred.ProjectRed;
import net.minecraftforge.client.MinecraftForgeClient;

public class IntegrationClientProxy extends IntegrationProxy {
	@Override
	public void init() {
		super.init();
		MinecraftForgeClient.registerItemRenderer(ProjectRed.itemPartGate.itemID, GateStaticRenderer.instance);
	}

}
