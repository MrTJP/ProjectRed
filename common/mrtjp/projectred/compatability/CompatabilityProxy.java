package mrtjp.projectred.compatability;

import mrtjp.projectred.core.IProxy;

public class CompatabilityProxy implements IProxy {

    @Override
    public void preinit() {
        Services.loadServices();
    }

    @Override
    public void init() {
        // Tinkers Construct
        if (Services.loadTConstruct)
            Services.getTCProxy().loadTCInteractions();
    }

    @Override
    public void postinit() {

    }

}
