package mrtjp.projectred.integration;

import static mrtjp.projectred.ProjectRed.itemScrewdriver;
import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.core.Configurator;
import net.minecraftforge.client.MinecraftForgeClient;
import codechicken.lib.packet.PacketCustom;
import cpw.mods.fml.common.registry.LanguageRegistry;

public class IntegrationClientProxy extends IntegrationProxy {
	@Override
	public void init() {
		MinecraftForgeClient.registerItemRenderer(ProjectRed.itemPartGate.itemID, GateStaticRenderer.instance);

		for (EnumGate g : EnumGate.VALUES) {
			LanguageRegistry.addName(g.getItemStack(), g.name);
		}
		LanguageRegistry.addName(itemScrewdriver, "Screwdriver");
		
		PacketCustom.assignHandler(Configurator.integrationPacketChannel, 0, 32, new IntegrationCPH());
	}

}
