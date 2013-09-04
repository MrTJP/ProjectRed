package mrtjp.projectred.integration;

<<<<<<< HEAD
import mrtjp.projectred.ProjectRedIntegration;
import net.minecraftforge.client.MinecraftForgeClient;
import codechicken.lib.packet.PacketCustom;
=======
import static mrtjp.projectred.ProjectRedIntegration.itemScrewdriver;
import mrtjp.projectred.ProjectRedIntegration;
import net.minecraftforge.client.MinecraftForgeClient;
import codechicken.lib.packet.PacketCustom;
import cpw.mods.fml.common.registry.LanguageRegistry;
>>>>>>> upstream/master

public class IntegrationClientProxy extends IntegrationProxy {

    @Override
    public void preinit() {
        super.preinit();
        PacketCustom.assignHandler(IntegrationCPH.channel, new IntegrationCPH());
    }
    
    @Override
    public void init() {
        super.init();
        MinecraftForgeClient.registerItemRenderer(ProjectRedIntegration.itemPartGate2.itemID, GateItemRenderer.instance);
        
        /*for (EnumGate g : EnumGate.VALID_GATES)
            if(g.implemented())
                LanguageRegistry.addName(g.getItemStack(), g.name);
        
        LanguageRegistry.addName(itemScrewdriver, "Screwdriver");*/
    }
}
