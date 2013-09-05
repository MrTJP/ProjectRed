package mrtjp.projectred.integration;

import static mrtjp.projectred.ProjectRedIntegration.itemPartGate;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProxy;
import codechicken.lib.packet.PacketCustom;
import codechicken.multipart.MultiPartRegistry;
import codechicken.multipart.MultiPartRegistry.IPartFactory;
import codechicken.multipart.TMultiPart;

public class IntegrationProxy implements IProxy, IPartFactory {

    @Override
    public void preinit() {
        PacketCustom.assignHandler(IntegrationSPH.channel, new IntegrationSPH());
    }
    
    @Override
    public void init() {
        MultiPartRegistry.registerParts(this, new String[]{
                "pr_sgate", "pr_igate", "pr_agate", "pr_bgate", "pr_tgate", "pr_rgate"});

        itemPartGate = new ItemPartGate(Configurator.part_gate.getInt());
    }

    @Override
    public void postinit() {
        IntegrationRecipes.initIntegrationRecipes();
    }

    @Override
    public TMultiPart createPart(String id, boolean client) {
        if(id.equals("pr_sgate"))
            return new SimpleGatePart();
        if(id.equals("pr_igate"))
            return new InstancedRsGatePart();
        if(id.equals("pr_bgate"))
            return new BundledGatePart();
        if(id.equals("pr_agate"))
            return new ArrayGatePart();
        if(id.equals("pr_tgate"))
            return new InstancedRsGatePartT();
        if(id.equals("pr_rgate"))
            return new RowGatePart();
        return null;
    }
}
