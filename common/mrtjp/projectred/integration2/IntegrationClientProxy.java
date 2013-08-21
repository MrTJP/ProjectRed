package mrtjp.projectred.integration2;

import mrtjp.projectred.ProjectRedIntegration;
import net.minecraftforge.client.MinecraftForgeClient;
import cpw.mods.fml.common.registry.LanguageRegistry;

public class IntegrationClientProxy extends IntegrationProxy {

    @Override
    public void init() {
        super.init();
        MinecraftForgeClient.registerItemRenderer(ProjectRedIntegration.itemPartGate2.itemID, GateItemRenderer.instance);
        
        for (EnumGate g : EnumGate.VALID_GATES)
            LanguageRegistry.addName(g.getItemStack(), g.name);
    }
}
