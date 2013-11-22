package mrtjp.projectred.expansion;

import static mrtjp.projectred.ProjectRedExpansion.itemPartPipe;
import net.minecraftforge.client.MinecraftForgeClient;
import codechicken.lib.packet.PacketCustom;

public class ExpansionClientProxy extends ExpansionProxy {
    
    @Override
    public void preinit() {
        super.preinit();
        PacketCustom.assignHandler(ExpansionCPH.channel, new ExpansionCPH());
    }
    
    @Override
    public void init() {
        super.init();
        MinecraftForgeClient.registerItemRenderer(itemPartPipe.itemID, PipeItemRenderer.instance);
    }

    @Override
    public void postinit() {
        super.postinit();
        ExpansionRecipes.initRecipes();
    }
}
