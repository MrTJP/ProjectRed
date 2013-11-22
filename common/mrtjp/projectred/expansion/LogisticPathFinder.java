package mrtjp.projectred.expansion;

import java.util.BitSet;

import mrtjp.projectred.core.utils.ItemKey;

public class LogisticPathFinder {

    private final Router source;
    private SyncResponse result;

    private BitSet exclusions = new BitSet();
    private BitSet visited;
    
    private boolean excludeSource;
    
    private ItemKey payload;
    
    public LogisticPathFinder(Router source, ItemKey payload) {
        this.source = source;
        this.payload = payload;
        this.visited = new BitSet(Router.getIPEndPool());
    }
    
    public LogisticPathFinder setExclusions(BitSet exclusions) {
        this.exclusions = exclusions;
        return this;
    }
    public LogisticPathFinder setExcludeSource(boolean excludeSource) {
        this.excludeSource = excludeSource;
        return this;
    }
    
    public SyncResponse getResult() {
        return result;
    }
    
    public LogisticPathFinder findBestResult() {
        SyncResponse bestResponse = new SyncResponse();
        int bestIP = -1;
        
        for (Router r : source.getRoutersByCost()) {
            
            if (excludeSource && r.getIPAddress() == source.getIPAddress())
                continue;
            
            if (exclusions.get(r.getIPAddress()) || visited.get(r.getIPAddress()))
                continue;
            
            visited.set(r.getIPAddress());
            
            IWorldRouter parent = r.getParent();
            
            if (parent == null) continue;
            
            SyncResponse sync = parent.getSyncResponse(payload, bestResponse);
            
            if (sync != null) {
                if (sync.priority.ordinal() > bestResponse.priority.ordinal()) {
                    bestResponse = sync;
                    bestIP = r.getIPAddress();
                } else if (sync.priority.ordinal() == bestResponse.priority.ordinal() && sync.customPriority > bestResponse.customPriority) {
                    bestResponse = sync;
                    bestIP = r.getIPAddress();
                }
            }
        }
        
        if (bestIP > -1)
            result = bestResponse.setResponder(bestIP);
        
        return this;
    }
}
