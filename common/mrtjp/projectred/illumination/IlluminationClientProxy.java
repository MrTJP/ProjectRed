package mrtjp.projectred.illumination;

import codechicken.lib.packet.PacketCustom;
import mrtjp.projectred.ProjectRedIllumination;
import mrtjp.projectred.core.CoreCPH;
import net.minecraftforge.client.MinecraftForgeClient;
import net.minecraftforge.common.MinecraftForge;
import cpw.mods.fml.client.registry.RenderingRegistry;

public class IlluminationClientProxy extends IlluminationProxy {

    public static int lampRenderID = 0;
    
    @Override
    public void init() {
        super.init();
        
		PacketCustom.assignHandler(IlluminationCPH.channel, new IlluminationCPH());
        
        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartLantern.itemID, RenderLantern.instance);
        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartInvLantern.itemID, RenderLantern.instance);

        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartIllumarButton.itemID, RenderIllumarButton.instance);

        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartCageLamp.itemID, RenderCageLamp.instance);
        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartInvCageLamp.itemID, RenderCageLamp.instance);

        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartFixture.itemID, RenderFixture.instance);
        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartInvFixture.itemID, RenderFixture.instance);

        lampRenderID = RenderingRegistry.getNextAvailableRenderId();
        RenderingRegistry.registerBlockHandler(LampISBRH.instance);
        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.blockLamp.blockID, LampISBRH.instance);
        
        MinecraftForge.EVENT_BUS.register(RenderHalo.instance);
    }
}
