package mrtjp.projectred.transmission;

import codechicken.multipart.TMultiPart;

public interface IWirePart
{
    public void updateAndPropogate(TMultiPart prev, boolean force);
}
