package mrtjp.projectred.transportation;

import java.util.ArrayList;
import java.util.List;

import net.minecraft.tileentity.TileEntity;
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
