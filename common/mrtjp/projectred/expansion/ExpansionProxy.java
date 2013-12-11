package mrtjp.projectred.expansion;

import mrtjp.projectred.core.IProxy;
import codechicken.multipart.MultiPartRegistry.IPartFactory;
import codechicken.multipart.TMultiPart;

public class ExpansionProxy implements IProxy, IPartFactory
{
    @Override
    public void preinit()
    {
    }

    @Override
    public void init()
    {
    }

    @Override
    public void postinit()
    {
        ExpansionRecipes.initRecipes();
    }

    @Override
    public TMultiPart createPart(String id, boolean arg1)
    {
        return null;
    }
}