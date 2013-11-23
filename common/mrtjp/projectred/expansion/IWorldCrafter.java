package mrtjp.projectred.expansion;

import java.util.List;

import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.core.utils.ItemKeyStack;
import mrtjp.projectred.expansion.RequestBranchNode.CraftingPromise;
import mrtjp.projectred.expansion.RequestBranchNode.DeliveryPromise;

public interface IWorldCrafter extends IWorldRequester, IWorldBroadcaster {

    public CraftingPromise requestCraftPromise(ItemKey item);
    
    public void registerExcess(DeliveryPromise promise);
    
    public List<ItemKeyStack> getCraftedItems();
    
    public int getWorkLoad();
}
