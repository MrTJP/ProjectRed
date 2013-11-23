package mrtjp.projectred.expansion;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.core.utils.ItemKeyStack;

public class CollectionPathFinder {

    private boolean collectBroadcasts = false;
    private boolean collectCrafts = false;
    private IWorldRequester requester;
    
    private Map<ItemKey, Integer> collected = null;
    
    public CollectionPathFinder() {
    }
    
    public CollectionPathFinder setCollectBroadcasts(boolean flag) {
        collectBroadcasts = flag;
        return this;
    }
    
    public CollectionPathFinder setCollectCrafts(boolean flag) {
        collectCrafts = flag;
        return this;
    }
    
    public CollectionPathFinder setRequester(IWorldRequester requester) {
        this.requester = requester;
        return this;
    }
    
    public CollectionPathFinder collect() {
        Map<ItemKey, Integer> collected = new HashMap<ItemKey, Integer>();
        for (Router r : requester.getRouter().getRoutersByCost()) {
            IWorldRouter wr = r.getParent();
            if (wr instanceof IWorldCrafter) {
                IWorldCrafter c = (IWorldCrafter) wr;
                c.getBroadcastedItems(collected);
                List<ItemKeyStack> list = c.getCraftedItems();
                if (list != null) {
                    for (ItemKeyStack stack : list)
                        if (!collected.containsKey(stack.key()))
                            collected.put(stack.key(), null);
                }

            } else if (wr instanceof IWorldBroadcaster) {
                IWorldBroadcaster b = (IWorldBroadcaster) wr;
                b.getBroadcastedItems(collected);
            }
        }
        
        this.collected = collected;
        return this;
    }
    
    public Map<ItemKey, Integer> getCollection() {
        return collected;
    }
}
