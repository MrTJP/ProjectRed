package mrtjp.projectred.expansion;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.Collections;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.PriorityQueue;
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
        
    /** List of StartEndPath sorted by distance, closest one having the lowest index **/
    private List<StartEndPath> routersByCost = Collections.unmodifiableList(new LinkedList<StartEndPath>());

    /** Set of known exits from here. Used to determine render color of a side **/
    private EnumSet<ForgeDirection> routedExits = EnumSet.noneOf(ForgeDirection.class);
    /** Link State for all nodes that branch from this one **/
    private Map<Router, StartEndPath> adjacentLinks = new HashMap<Router, StartEndPath>();

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
        
        HashMap<Router, StartEndPath> newAdjacent;
        LSPathFinder finder = new LSPathFinder(wr, Configurator.maxDetectionCount, Configurator.maxDetectionLength);
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
            for (Entry<Router, StartEndPath> r : newAdjacent.entrySet()) {
                StartEndPath oldEdge = adjacentLinks.get(r.getKey());
                if (oldEdge == null) {
                    adjacentChanged = true;
                    break;
                }

                StartEndPath newEdge = r.getValue();
                if (!newEdge.equals(oldEdge)) {
                    adjacentChanged = true;
                    break;
                }
            }
        
        // Update local LSA
        if (adjacentChanged) {
            EnumSet<ForgeDirection> newExits = EnumSet.noneOf(ForgeDirection.class);
            for(Entry<Router,StartEndPath> r : newAdjacent.entrySet())
                newExits.add(ForgeDirection.getOrientation(r.getValue().dirToFirstHop));
            
            adjacentLinks = Collections.unmodifiableMap(newAdjacent);
            routedExits = newExits;

            HashMap<Router, Integer> neighboursWithCost = new HashMap<Router, Integer>();
            for (Entry<Router, StartEndPath> r : adjacentLinks.entrySet())
                neighboursWithCost.put(r.getKey(), r.getValue().distance);

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
    
    public List<StartEndPath> getRoutersByCost() {
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
        
        /** Map of router and its rudimentary path **/
        HashMap<Router, StartEndPath> tree2 = new HashMap<Router, StartEndPath>();
        /** Queue of all candidates that need checking **/
        PriorityQueue<StartEndPath> candidates2 = new PriorityQueue<StartEndPath>();
        /** List of all routers connected to this network ordered by cost **/
        ArrayList<StartEndPath> routersByCost2 = new ArrayList<StartEndPath>();
        
        // Start by adding our info.
        tree2.put(this, new StartEndPath(this, this, -1, 0));
        for (Router r : adjacentLinks.keySet()) {
        	StartEndPath l = adjacentLinks.get(r);
        	candidates2.add(new StartEndPath(l.start, l.end, l.dirToFirstHop, l.distance));
        }
        
        SharedLSADatabasereadLock.lock();
        StartEndPath nextLowest = null;
        while((nextLowest = candidates2.poll()) != null) {
        	// We already approved this router. Keep skipping until we get a fresh one.
        	while (nextLowest != null && tree2.containsKey(nextLowest.end))
        		nextLowest = candidates2.poll();
        	
        	// No more routers to act on. We are done.
        	if (nextLowest == null)
        		break;
        	
        	// This is the lowest path so far to here. 
        	StartEndPath lowestPath = tree2.get(nextLowest.start);
        	// If its null, then we are the start of the path to this router.
        	if (lowestPath == null)
        		lowestPath = nextLowest;
        	
        	// Add all of our neighbors so they are checked.
            LSA lsa = null;
            if(nextLowest.end.getIPAddress() < SharedLSADatabase.length)
                lsa = SharedLSADatabase[nextLowest.end.getIPAddress()];
            
            if (lsa != null) {
				for (Router r : ((Map<Router, Integer>) lsa.neighbors.clone()).keySet()) {
					if (tree2.containsKey(r))
						continue;
					
					int newCost = nextLowest.distance + lsa.neighbors.get(r);
					candidates2.add(new StartEndPath(lowestPath.end, r, lowestPath.dirToFirstHop, newCost));
				}
            }
            
            // Approve this candidate
            nextLowest.start = lowestPath.start;
            tree2.put(nextLowest.end, nextLowest);
            routersByCost2.add(nextLowest);
        }
        SharedLSADatabasereadLock.unlock();
        
        HashMap<Router, ForgeDirection> routeTable2 = new HashMap<Router, ForgeDirection>(tree2.size());
        for (StartEndPath l : tree2.values()) {
        	Router firstHop = l.start;
        	if (firstHop == null) {
        		routeTable2.put(l.end, ForgeDirection.UNKNOWN);
        		continue;
        	}
        	
        	StartEndPath localOutPath = adjacentLinks.get(firstHop);
        	if (localOutPath == null)
        		continue;
        	
        	routeTable2.put(l.end, ForgeDirection.getOrientation(localOutPath.dirToFirstHop));
        }

        // Set the new routing tables.
        routingTableUpdateWriteLock.lock();
        if(newVersion == LSAVersion){
            SharedLSADatabasereadLock.lock();
            if(LSALegacyVersion[IPAddress] < newVersion){
                LSALegacyVersion[IPAddress] = newVersion;
                routeTable = Collections.unmodifiableMap(routeTable2);
                routersByCost = Collections.unmodifiableList(routersByCost2);
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
    
    private static class LSA {
	    public HashMap<Router, Integer> neighbors = new HashMap<Router, Integer>();
	}

	public static class StartEndPath implements Comparable<StartEndPath> {
        public int dirToFirstHop;
        public int distance;
        public Router start;
        public final Router end;

        public StartEndPath(Router start, Router end, int dir, int length) {
            this.start = start;
            this.end = end;
            
            this.dirToFirstHop = dir;
            this.distance = length;
        }

		@Override
		public boolean equals(Object o) {			
			if (o instanceof StartEndPath) {
				StartEndPath p = (StartEndPath) o;
				return dirToFirstHop == p.dirToFirstHop
						&& distance == p.distance;
			}
			return false;
		}

		@Override
		public int compareTo(StartEndPath o) {
			int c = distance - o.distance;
			if (c == 0)
				c = end.getIPAddress() - o.end.getIPAddress();
			return c;
		}
    }
}
