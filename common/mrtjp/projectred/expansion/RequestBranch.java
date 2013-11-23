package mrtjp.projectred.expansion;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import mrtjp.projectred.core.utils.HashPair2;
import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.core.utils.ItemKeyStack;

public class RequestBranch extends RequestBranchNode {

	private HashMap<HashPair2<IWorldBroadcaster, ItemKey>, Integer> totalPromises;

	enum RequestFlags {
		PULL,
		CRAFT,
		PARTIALS,
		SIMULATE,
		;
		
		public static final EnumSet<RequestFlags> all = EnumSet.allOf(RequestFlags.class);
		public static final EnumSet<RequestFlags> def = EnumSet.of(PULL, CRAFT);
	}
    
	public RequestBranch(ItemKeyStack requestedPackage, IWorldRequester requester, EnumSet<RequestFlags> type) {
		super(null, requestedPackage, requester, null, type);
	}
    
    protected void promiseAdded(DeliveryPromise promise) {
        HashPair2<IWorldBroadcaster, ItemKey> key = new HashPair2<IWorldBroadcaster, ItemKey>(promise.sender, promise.thePackage);
        totalPromises.put(key, getExistingPromisesFor(key) + promise.size);
    }
    
    protected void promiseRemoved(DeliveryPromise promise) {
        HashPair2<IWorldBroadcaster, ItemKey> key = new HashPair2<IWorldBroadcaster, ItemKey>(promise.sender, promise.thePackage);
        int newCount = getExistingPromisesFor(key) - promise.size;
        if (newCount <= 0)
            totalPromises.remove(key);
        else
            totalPromises.put(key, newCount);
        
    }
    
    protected LinkedList<ExcessPromise> getAllExcessFor(ItemKey item) {
    	HashMap<IWorldBroadcaster, List<ExcessPromise>> excessMap = new HashMap<IWorldBroadcaster, List<ExcessPromise>>();
    	recurse_GatherExcess(item, excessMap);
    	recurse_RemoveUnusableExcess(item, excessMap);
    	LinkedList<ExcessPromise> excessPromises = new LinkedList<ExcessPromise>();
    	
    	for (List<ExcessPromise> list : excessMap.values())
    		excessPromises.addAll(list);
    	
    	return excessPromises;
    }
    
    public int getExistingPromisesFor(HashPair2<IWorldBroadcaster, ItemKey> key) {
        if (totalPromises == null)
            totalPromises = new HashMap<HashPair2<IWorldBroadcaster, ItemKey>, Integer>();
        Integer n = totalPromises.get(key);
        if (n == null)
            return 0;
        return n.intValue();
    }
}
