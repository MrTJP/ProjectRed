package mrtjp.projectred.expansion;

import java.util.BitSet;
import java.util.Collections;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.UUID;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.expansion.TableUpdateThread.RouteLayerUpdater;
import net.minecraft.world.World;
import net.minecraftforge.common.DimensionManager;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.TMultiPart;

public class Router implements Comparable<Router> {

    /** Mapped [router, orientation] for connected destinations **/
    private Map<Router, ForgeDirection> routeTable = Collections.unmodifiableMap(new HashMap<Router, ForgeDirection>());
    /** Mapped [router, distance] for connected destinations **/
    private Map<Router, Integer> routeCosts = Collections.unmodifiableMap(new HashMap<Router, Integer>());
    /** List of Routers sorted by distance, closest one having the lowest index **/
    private List<Router> routersByCost = Collections.unmodifiableList(new LinkedList<Router>());
    /** Set of known exits from here. Used to determine render color of a side **/
    private EnumSet<ForgeDirection> routedExits = EnumSet.noneOf(ForgeDirection.class);
    /** Link State for all nodes that branch from this one **/
    private Map<Router, NodeLink> adjacentLinks = new HashMap<Router, NodeLink>();

    /** Locks for allowing thread-safe read-write access to router information **/
    protected static final ReentrantReadWriteLock SharedLSADatabaseLock = new ReentrantReadWriteLock();
    protected static final Lock SharedLSADatabasereadLock = SharedLSADatabaseLock.readLock();
    protected static final Lock SharedLSADatabasewriteLock = SharedLSADatabaseLock.writeLock();
    protected final ReentrantReadWriteLock routingTableUpdateLock = new ReentrantReadWriteLock();
    protected final Lock routingTableUpdateReadLock = routingTableUpdateLock.readLock();
    protected final Lock routingTableUpdateWriteLock = routingTableUpdateLock.writeLock();

    /*** Link State ***/
    private static int[] LSALegacyVersion = new int[0];
    private int LSAVersion = 0;
    private static LSA[] SharedLSADatabase = new LSA[0];
    private LSA LSA; 

    /** Identification **/
    private final int IPAddress;
    private final UUID ID;
    private final int dim;
    private final BlockCoord location;
    private static int nextIP = 1;
    private static BitSet usedIPs = new BitSet();

    public Router(UUID id, int dim, BlockCoord bc) {
        if(id != null) 
            this.ID = id;
        else 
            this.ID = UUID.randomUUID();
        
        this.dim = dim;
        this.location = bc;
        
        LSA = new LSA();
        
        SharedLSADatabasewriteLock.lock();
        
        this.IPAddress = claimIPAddress();
        if(SharedLSADatabase.length <= IPAddress){
            int newlength = ((int) (IPAddress*1.5))+1;
            LSA[] new_SharedLSADatabase = new LSA[newlength];
            System.arraycopy(SharedLSADatabase, 0, new_SharedLSADatabase, 0, SharedLSADatabase.length);
            SharedLSADatabase = new_SharedLSADatabase;
            int[] new_lastLSAVersion = new int[newlength];
            System.arraycopy(LSALegacyVersion, 0, new_lastLSAVersion, 0, LSALegacyVersion.length);
            LSALegacyVersion = new_lastLSAVersion;
        }
        LSALegacyVersion[IPAddress] = 0;
        SharedLSADatabase[IPAddress] = LSA;
        
        SharedLSADatabasewriteLock.unlock();
    }
    
    public void update(boolean force) {
        if (force) {
            if (updateLSAIfNeeded())
                startLSAFloodfill();

            refreshRouteTableIfNeeded(false);

            IWorldRouter r = getParent();
            if (r != null)
                r.refreshState();
            
            return;
        }
        
        if (Configurator.routerUpdateThreadCount > 0)
            refreshRouteTableIfNeeded(false);
    }
    
    private boolean updateLSAIfNeeded() {
        boolean adjacentChanged = false;
        IWorldRouter wr = getParent();
        if (wr == null) return false;
        
        HashMap<Router, NodeLink> newAdjacent;
        LSPathFinder finder = new LSPathFinder(wr.getContainer(), Configurator.maxDetectionCount, Configurator.maxDetectionLength);
        newAdjacent = finder.result;

        for (Router r : newAdjacent.keySet()) {
            if (r.getParent().needsWork())
                return false;
        }
        
        if(adjacentLinks.size() != newAdjacent.size()) // Different number of connections
            adjacentChanged = true;
        
        else if (!adjacentChanged) // Members missing
            for (Router r : adjacentLinks.keySet()) {
                if (!newAdjacent.containsKey(r)) {
                    adjacentChanged = true;
                    break;
                }
            }
        
        else if (!adjacentChanged) // Edges different
            for (Entry<Router, NodeLink> r : newAdjacent.entrySet()) {
                NodeLink oldEdge = adjacentLinks.get(r.getKey());
                if (oldEdge == null) {
                    adjacentChanged = true;
                    break;
                }

                NodeLink newEdge = r.getValue();
                if (!newEdge.equals(oldEdge)) {
                    adjacentChanged = true;
                    break;
                }
            }
        
        // Update local LSA
        if (adjacentChanged) {
            EnumSet<ForgeDirection> newExits = EnumSet.noneOf(ForgeDirection.class);
            for(Entry<Router,NodeLink> r : newAdjacent.entrySet())
                newExits.add(r.getValue().outOrient);
            
            adjacentLinks = Collections.unmodifiableMap(newAdjacent);
            routedExits = newExits;

            HashMap<Router, Integer> neighboursWithCost = new HashMap<Router, Integer>();
            for (Entry<Router, NodeLink> r : adjacentLinks.entrySet())
                neighboursWithCost.put(r.getKey(), r.getValue().outDistance);

            SharedLSADatabasewriteLock.lock();
            LSA.neighbors = neighboursWithCost;
            SharedLSADatabasewriteLock.unlock();
        }
        
        return adjacentChanged;
    }

    private void startLSAFloodfill() {
        BitSet prev = new BitSet(getIPEndPool());
        prev.set(IPAddress);

        for(Router r : adjacentLinks.keySet())
            r.LSAUpdateFloodfill(prev);

        prev.clear();
        prev.set(IPAddress);
        
        flagForRoutingUpdate();

        for(Router r : adjacentLinks.keySet())
            r.adjacentUpdateFloodfill(prev);
    }
    
    public void LSAUpdateFloodfill(BitSet prev) {
        if (prev.get(IPAddress))
            return;
        
        prev.set(IPAddress);
        
        updateLSAIfNeeded();
        //getParent().refreshState();
        
        for(Router r : adjacentLinks.keySet())
            r.LSAUpdateFloodfill(prev);
    }
    
    public void adjacentUpdateFloodfill(BitSet prev) {
        if (prev.get(IPAddress))
            return;
        
        prev.set(IPAddress);
        
        flagForRoutingUpdate();
        
        for(Router r : adjacentLinks.keySet())
            r.adjacentUpdateFloodfill(prev);
    }
    
    public void flagForRoutingUpdate() {
        LSAVersion++;
    }

    public void decommission() {
        SharedLSADatabasewriteLock.lock();
        if (IPAddress < SharedLSADatabase.length)
            SharedLSADatabase[IPAddress] = null;
        SharedLSADatabasewriteLock.unlock();
        
        RouterServices.instance.removeRouter(IPAddress);

        startLSAFloodfill();
        releaseIPAddress(IPAddress);
    }

    public int getLSAVersion() {
        return LSAVersion;
    }

    public int getIPAddress() {
        return IPAddress;
    }

    public UUID getID() {
        return ID;
    }
    
    public Map<Router, ForgeDirection> getRouteTable(){
        refreshRouteTableIfNeeded(true);
        return routeTable;
    }
    
    public List<Router> getRoutersByCost() {
        refreshRouteTableIfNeeded(true);
        return routersByCost;
    }
    
    public ForgeDirection getExitDirection(int destination) {
        if (!RouterServices.instance.doesRouterExist(destination))
            return ForgeDirection.UNKNOWN;
                
        ForgeDirection dir = getRouteTable().get(RouterServices.instance.getRouter(destination));
        if (dir == null)
            return ForgeDirection.UNKNOWN;

        return dir;
    }
    
    public boolean canRouteTo(int destination) {
        if (!RouterServices.instance.doesRouterExist(destination))
            return false;

        ForgeDirection dir = getRouteTable().get(RouterServices.instance.getRouter(destination));
        if (dir == null || dir == ForgeDirection.UNKNOWN)
            return false;
        
        return true;
    }

    private void refreshRouteTableIfNeeded(boolean force){
        if (LSAVersion > LSALegacyVersion[IPAddress]) {
            if(Configurator.routerUpdateThreadCount > 0 && !force)
                TableUpdateThread.add(new RouteLayerUpdater(this));
            else
                refreshRoutingTable(LSAVersion);
        }
    }

    Comparator c = new Comparator<Entry<Router, Integer>>() {
        @Override
        public int compare(Entry<Router, Integer> o1, Entry<Router, Integer> o2) {
            return o1.getValue() > o2.getValue() ? 1 : -1;
        }
    };

    public void refreshRoutingTable(int newVersion) {
        if(LSALegacyVersion[IPAddress] >= newVersion)
            return;
                
        /** Map of all "approved" routers and the route to get there **/
        HashMap<Router, LinkedList<Router>> tree =  new HashMap<Router, LinkedList<Router>>();
        /** The cost to get to an "approved" router **/
        HashMap<Router, Integer> treeCost = new HashMap<Router, Integer>();
        
        tree.put(this,  new LinkedList<Router>());
        treeCost.put(this,  0);

        /** The candidate router and which approved router put it in the candidate list **/
        HashMap<Router, Router> candidates = new HashMap<Router, Router>();
        /** The total cost for the candidate route **/
        HashMap<Router, Integer> candidatesCost = new HashMap<Router, Integer>();
        
        // Start at all adjacent routers.
        for (Router r : adjacentLinks.keySet()) {
            candidates.put(r, this);
            candidatesCost.put(r, adjacentLinks.get(r).outDistance);
        }

        SharedLSADatabasereadLock.lock();
        while (!candidates.isEmpty()) {
            // Get next lowest candidate router.
            Router lowest = null;
            int lowestCost = Integer.MAX_VALUE;
            for (Router r : candidatesCost.keySet()) {
                int newCost = candidatesCost.get(r);
                if (newCost < lowestCost) {
                    lowest = r;
                    lowestCost = newCost;
                }
            }

            // Get the approved parent of the lowest cost candidate
            Router lowestParent = candidates.get(lowest);
            // Get a copy of the route for the approved router
            LinkedList<Router> lowestPath = (LinkedList<Router>) tree.get(lowestParent).clone();
            // Add to the route to get to the candidate
            lowestPath.addLast(lowest);

            // Approve the candidate
            tree.put(lowest, lowestPath);
            treeCost.put(lowest, lowestCost);
            
            // Remove as candidate
            candidates.remove(lowest);
            candidatesCost.remove(lowest);
            
            LSA lsa = null;
            if(lowest.getIPAddress() < SharedLSADatabase.length)
                lsa = SharedLSADatabase[lowest.getIPAddress()];

            if (lsa == null)
                continue;
            
            // Add all linked neighbors of this newly approved router.
            for (Entry<Router, Integer> entry : ((Map<Router, Integer>)lsa.neighbors.clone()).entrySet()) {
                Router newCandidate = entry.getKey();
                
                if (tree.containsKey(newCandidate))
                    continue;
                
                int newCandidateCost = lowestCost + entry.getValue();

                if (candidates.containsKey(newCandidate) && candidatesCost.get(newCandidate) <= newCandidateCost)
                    continue;
                
                candidates.put(newCandidate, lowest);
                candidatesCost.put(newCandidate, newCandidateCost);
            }
        }
        SharedLSADatabasereadLock.unlock();

        
        // Build the routing table from the approved routers.
        Map<Router, ForgeDirection> _routeTable = new HashMap<Router, ForgeDirection>(getIPEndPool());
        Map<Router, Integer> _routeCosts = new HashMap<Router, Integer>(getIPEndPool());
        LinkedList _routersByCost = new LinkedList<Router>();
        for (Router node : tree.keySet()) {
            LinkedList<Router> route = tree.get(node);
            if (route.size() == 0) {
                _routeTable.put(node, ForgeDirection.UNKNOWN);
                continue;
            }
            
            Router firstHop = route.getFirst();
            if (firstHop == null) continue;
            
            if (!adjacentLinks.containsKey(firstHop))
                continue;
            
            _routeCosts.put(node, treeCost.get(node));
            _routeTable.put(node, adjacentLinks.get(firstHop).outOrient);
        }

        // Sort routers into a list by distance
        List<Entry<Router, Integer>> temp = new LinkedList<Entry<Router, Integer>>(_routeCosts.entrySet());
                
        Collections.sort(temp, c);
        
        for (Entry<Router, Integer> e : temp)
            _routersByCost.addLast(e.getKey());
        
        // Set the new routing tables.
        routingTableUpdateWriteLock.lock();
        if(newVersion == LSAVersion){
            SharedLSADatabasereadLock.lock();
            if(LSALegacyVersion[IPAddress] < newVersion){
                LSALegacyVersion[IPAddress] = newVersion;
                routeTable = Collections.unmodifiableMap(_routeTable);
                routeCosts = Collections.unmodifiableMap(_routeCosts);
                routersByCost = Collections.unmodifiableList(_routersByCost);
            }
            SharedLSADatabasereadLock.unlock();
        }
        routingTableUpdateWriteLock.unlock();
    }
    
    public IWorldRouter getParent() {
        TMultiPart p = BasicUtils.getMultiPart(getWorld(), location, 6);
        if (p instanceof IWorldRouter)
            return (IWorldRouter) p;
        return null;
    }

    public World getWorld() {
        return DimensionManager.getWorld(dim);
    }

    private static int claimIPAddress() {
        int ip = usedIPs.nextClearBit(nextIP);
        nextIP = ip + 1;
        usedIPs.set(ip);
        return ip;
    }
    private static void releaseIPAddress(int ip) {
        usedIPs.clear(ip);
        if(ip < nextIP)
            nextIP = ip;
    }
    public static int getIPEndPool() {
        return usedIPs.size();
    }
    
    @Override 
    public int hashCode(){
        return IPAddress;
    }

    private static class LSA {
        public HashMap<Router, Integer> neighbors = new HashMap<Router, Integer>();
    }

    public int getDim() {
        return dim;
    }

    public BlockCoord getLocation() {
        return location;
    }

    public boolean LSAExists(ForgeDirection dir) {
        return routedExits.contains(dir);
    }

    public int compareTo(Router o) {
        return IPAddress - o.getIPAddress();
    }
    
    public static void reboot() {
        SharedLSADatabasewriteLock.lock();
        SharedLSADatabase = new LSA[0];
        LSALegacyVersion = new int[0];
        SharedLSADatabasewriteLock.unlock();
        usedIPs.clear();
        nextIP = 1;
    }
}
