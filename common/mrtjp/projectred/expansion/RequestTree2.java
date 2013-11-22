package mrtjp.projectred.expansion;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import mrtjp.projectred.core.utils.HashPair2;
import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.core.utils.ItemKeyStack;

public class RequestTree2 extends RequestTreeNode2 {

    enum RequestFlags {
        PULL,
        CRAFT,
        PARTIALS,
        SIMULATE
    }

    public RequestTree2(ItemKeyStack requestedPackage, IWorldRoutedRequester requester, EnumSet<RequestFlags> type) {
        super(null, requestedPackage, requester, null, type);
    }
    
    private HashMap<HashPair2<IWorldRoutedBroadcaster, ItemKey>, Integer> totalPromises;
    
    protected LinkedList<ExcessPromise> getAllExcessFor(ItemKey item) {
        HashMap<IWorldRoutedBroadcaster, List<ExcessPromise>> excessMap = new HashMap<IWorldRoutedBroadcaster, List<ExcessPromise>>();
        recurse_GatherExcess(item, excessMap);
        recurse_RemoveUnusableExcess(item, excessMap);
        LinkedList<ExcessPromise> excessPromises = new LinkedList<ExcessPromise>();
        
        for (List<ExcessPromise> list : excessMap.values())
            excessPromises.addAll(list);
        
        return excessPromises;
    }
    
    protected void promiseAdded(DeliveryPromise promise) {
        HashPair2<IWorldRoutedBroadcaster, ItemKey> key = new HashPair2<IWorldRoutedBroadcaster, ItemKey>(promise.sender, promise.thePackage);
        totalPromises.put(key, getExistingPromisesFor(key) + promise.deliveryCount);
    }
    
    protected void promiseRemoved(DeliveryPromise promise) {
        HashPair2<IWorldRoutedBroadcaster, ItemKey> key = new HashPair2<IWorldRoutedBroadcaster, ItemKey>(promise.sender, promise.thePackage);
        int newCount = getExistingPromisesFor(key) - promise.deliveryCount;
        if (newCount <= 0)
            totalPromises.remove(key);
        else
            totalPromises.put(key, newCount);
        
    }
    
    public int getExistingPromisesFor(HashPair2<IWorldRoutedBroadcaster, ItemKey> key) {
        if (totalPromises == null)
            totalPromises = new HashMap<HashPair2<IWorldRoutedBroadcaster, ItemKey>, Integer>();
        Integer n = totalPromises.get(key);
        if (n == null)
            return 0;
        return n.intValue();
    }
}
