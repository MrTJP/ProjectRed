package mrtjp.projectred.integration;

import mrtjp.projectred.ProjectRedIntegration;
import net.minecraftforge.client.MinecraftForgeClient;
import codechicken.lib.packet.PacketCustom;

public class IntegrationClientProxy extends IntegrationProxy {

    @Override
    public void preinit() {
        super.preinit();
        PacketCustom.assignHandler(IntegrationCPH.channel, new IntegrationCPH());
    }
    
    @Override
    public void init() {
        super.init();
        MinecraftForgeClient.registerItemRenderer(ProjectRedIntegration.itemPartGate.itemID, GateItemRenderer.instance);
        
        /*for (EnumGate g : EnumGate.VALID_GATES)
            if(g.implemented())
                LanguageRegistry.addName(g.getItemStack(), g.name);
        */
    }
}
