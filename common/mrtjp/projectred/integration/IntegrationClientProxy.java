package mrtjp.projectred.integration;

import cpw.mods.fml.common.registry.LanguageRegistry;
import mrtjp.projectred.ProjectRed;
import net.minecraftforge.client.MinecraftForgeClient;

public class IntegrationClientProxy extends IntegrationProxy {
	@Override
	public void init() {
		super.init();
		MinecraftForgeClient.registerItemRenderer(ProjectRed.itemPartGate.itemID, GateStaticRenderer.instance);
		for (EnumGate g : EnumGate.VALUES) {
			LanguageRegistry.addName(g.getItemStack(), g.name);
		}
	}

}
