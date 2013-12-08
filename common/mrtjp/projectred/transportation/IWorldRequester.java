package mrtjp.projectred.transportation;

import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.core.utils.ItemKeyStack;

public interface IWorldRequester extends IWorldRouter 
{
    public void trackedItemLost(ItemKeyStack s);

    public void trackedItemReceived(ItemKeyStack s);
    
    public int getActiveFreeSpace(ItemKey item);
}
