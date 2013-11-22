package mrtjp.projectred.expansion;

import static mrtjp.projectred.ProjectRedExpansion.itemPartPipe;
import static mrtjp.projectred.ProjectRedExpansion.itemRoutingChip;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProxy;
import codechicken.lib.packet.PacketCustom;
import codechicken.multipart.MultiPartRegistry;
import codechicken.multipart.MultiPartRegistry.IPartFactory;
import codechicken.multipart.TMultiPart;

public class ExpansionProxy implements IProxy, IPartFactory {

    @Override
    public void preinit() {
        PacketCustom.assignHandler(ExpansionSPH.channel, new ExpansionSPH());
    }

    @Override
    public void init() {
        MultiPartRegistry.registerParts(this, new String[]{
                "pr_ptube",
                "pr_rbasic",
                "pr_rinterface",
                "pr_rcrafting",
                "pr_rrequest",
        });
        itemPartPipe = new ItemPartPipe(Configurator.part_pipe.getInt());
        itemRoutingChip = new ItemRoutingChip(Configurator.item_routingChipID.getInt());
        for (int i = 0; i < Configurator.routerUpdateThreadCount; i++)
            new TableUpdateThread(i);
        
        
    }

    @Override
    public void postinit() {
        ExpansionRecipes.initRecipes();
    }

    @Override
    public TMultiPart createPart(String id, boolean arg1) {
        if(id.equals("pr_ptube"))
            return new BasicPipePart();
        if(id.equals("pr_rbasic"))
            return new RoutedJunctionPipePart();
        if(id.equals("pr_rinterface"))
            return new RoutedInterfacePipePart();
        if(id.equals("pr_rcrafting"))
            return new RoutedCraftingPipePart();
        if(id.equals("pr_rrequest"))
            return new RoutedRequesterPipePart();
        return null;
    }

}
