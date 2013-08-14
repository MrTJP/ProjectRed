package mrtjp.projectred.integration;

import static mrtjp.projectred.ProjectRedCore.itemScrewdriver;
import mrtjp.projectred.ProjectRedIntegration;
import mrtjp.projectred.core.Configurator;
import net.minecraftforge.client.MinecraftForgeClient;
import codechicken.lib.packet.PacketCustom;
import cpw.mods.fml.common.registry.LanguageRegistry;

public class IntegrationClientProxy extends IntegrationProxy {

    @Override
    public void init() {
        super.init();
        MinecraftForgeClient.registerItemRenderer(ProjectRedIntegration.itemPartGate.itemID, GateStaticRenderer.instance);

        for (EnumGate g : EnumGate.VALID_GATES) {
            LanguageRegistry.addName(g.getItemStack(), g.name);
        }
        LanguageRegistry.addName(itemScrewdriver, "Screwdriver");
        
        PacketCustom.assignHandler(Configurator.integrationPacketChannel, 0, 32, new IntegrationCPH());
    }
}
