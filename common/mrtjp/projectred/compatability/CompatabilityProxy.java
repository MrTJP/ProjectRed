package mrtjp.projectred.compatability;

import mrtjp.projectred.core.IProxy;
import codechicken.multipart.MultiPartRegistry.IPartFactory;
import codechicken.multipart.TMultiPart;

public class CompatabilityProxy implements IProxy, IPartFactory {

    @Override
    public void preinit() {
    	Services.loadServices();
    }

    @Override
    public void init() {
    	Services.getTCProxy().loadTCInteractions();
    }

    @Override
    public void postinit() {

    }

    @Override
    public TMultiPart createPart(String id, boolean arg1) {
        return null;
    }

}
