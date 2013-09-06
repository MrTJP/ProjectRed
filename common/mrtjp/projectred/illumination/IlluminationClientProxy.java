package mrtjp.projectred.illumination;

import mrtjp.projectred.ProjectRedIllumination;
import net.minecraftforge.client.MinecraftForgeClient;
import net.minecraftforge.common.MinecraftForge;
import cpw.mods.fml.common.registry.LanguageRegistry;

public class IlluminationClientProxy extends IlluminationProxy {

    @Override
    public void init() {
        super.init();
        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartLantern.itemID, LanternRenderer.instance);
        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartInvLantern.itemID, LanternRenderer.instance);
        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartLamp.itemID, LampRenderer.instance);
        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartInvLamp.itemID, LampRenderer.instance);

        MinecraftForge.EVENT_BUS.register(LastEventBasedHaloRenderer.instance);
    }
}
