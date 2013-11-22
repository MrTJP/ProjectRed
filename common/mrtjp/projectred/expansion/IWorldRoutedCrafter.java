package mrtjp.projectred.expansion;

import java.util.List;

import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.core.utils.ItemKeyStack;
import mrtjp.projectred.expansion.RequestTreeNode2.CraftingPromise;
import mrtjp.projectred.expansion.RequestTreeNode2.DeliveryPromise;

public interface IWorldRoutedCrafter extends IWorldRoutedRequester, IWorldRoutedBroadcaster {

    public CraftingPromise requestCraftPromise(ItemKey item);
    
    public void registerExcess(DeliveryPromise promise);
    
    public List<ItemKeyStack> getCraftedItems();
    
    public int getWorkLoad();
}
