package mrtjp.projectred.transportation;

import static mrtjp.projectred.ProjectRedTransportation.itemPartPipe;
import net.minecraftforge.client.MinecraftForgeClient;
import codechicken.lib.packet.PacketCustom;

public class TransportationClientProxy extends TransportationProxy {

    @Override
    public void preinit() {
        super.preinit();
        PacketCustom.assignHandler(TransportationCPH.channel, new TransportationCPH());
    }

    @Override
    public void init() {
        super.init();
        MinecraftForgeClient.registerItemRenderer(itemPartPipe.itemID, PipeItemRenderer.instance);
    }

    @Override
    public void postinit() {
        super.postinit();
        TransportationRecipes.initRecipes();
    }
}
