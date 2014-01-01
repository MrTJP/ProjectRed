package mrtjp.projectred.compatibility;

import mrtjp.projectred.core.IProxy;

public class CompatibilityProxy implements IProxy
{
    @Override
    public void preinit()
    {
        Services.loadServices();
    }

    @Override
    public void init()
    {
        // Tinkers Construct
        if (Services.loadTConstruct)
            Services.getTCProxy().init();
        
        // Thermal Expansion
        if (Services.loadTExpansion)
            Services.getTEProxy().init();
    }

    @Override
    public void postinit()
    {
    }
}
