package mrtjp.projectred.illumination;

import mrtjp.projectred.ProjectRedIllumination;
import net.minecraftforge.client.MinecraftForgeClient;
import net.minecraftforge.common.MinecraftForge;
import codechicken.lib.packet.PacketCustom;
import cpw.mods.fml.client.registry.ClientRegistry;

public class IlluminationClientProxy extends IlluminationProxy
{
    @Override
    public void init()
    {
        super.init();

        PacketCustom.assignHandler(IlluminationCPH.channel, new IlluminationCPH());

        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartLantern.itemID, RenderLantern.instance);
        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartInvLantern.itemID, RenderLantern.instance);

        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartIllumarButton.itemID, RenderIllumarButton.instance);

        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartCageLamp.itemID, RenderCageLamp.instance);
        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartInvCageLamp.itemID, RenderCageLamp.instance);

        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartFixture.itemID, RenderFixture.instance);
        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartInvFixture.itemID, RenderFixture.instance);

        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.blockLamp.blockID, LampTESR.instance);
        ClientRegistry.bindTileEntitySpecialRenderer(TileLamp.class, LampTESR.instance);
        
        MinecraftForge.EVENT_BUS.register(RenderHalo.instance);
    }
}
