package mrtjp.projectred.transportation;

import mrtjp.projectred.api.ISpecialLinkState;
import mrtjp.projectred.api.ITransportationAPI;

public class APIImpl_Transportation implements ITransportationAPI
{
    @Override
    public void registerSpecialLinkState(ISpecialLinkState link)
    {
        LSPathFinder.register(link);
    }
}
