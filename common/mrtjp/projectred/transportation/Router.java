package mrtjp.projectred.transportation;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Iterator;
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
import net.minecraft.world.World;
import net.minecraftforge.common.DimensionManager;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.TMultiPart;

public class Router implements Comparable<Router>
{
    /** List of [ForgeDirection] indexed by IP for all routers in this network **/
    private List<ForgeDirection> routeTable = Collections.unmodifiableList(new ArrayList<ForgeDirection>());

    /** List of [StartEndPath] sorted by distance, closest one having the lowest index **/
    private List<StartEndPath> routersByCost = Collections.unmodifiableList(new LinkedList<StartEndPath>());

    /** Set of known exits from here. Used to determine render color of a side **/
    private EnumSet<ForgeDirection> routedExits = EnumSet.noneOf(ForgeDirection.class);

    /** Link State for all nodes that branch from this one **/
    private Map<Router, StartEndPath> adjacentLinks = new HashMap<Router, StartEndPath>();

    /*** Locks for allowing thread-safe read-write access to router information ***/
    protected static final ReentrantReadWriteLock LSADatabaseLock = new ReentrantReadWriteLock();
    protected static final Lock LSADatabasereadLock = LSADatabaseLock.readLock();
    protected static final Lock LSADatabasewriteLock = LSADatabaseLock.writeLock();
    protected final ReentrantReadWriteLock routingTableLock = new ReentrantReadWriteLock();
    protected final Lock routingTableReadLock = routingTableLock.readLock();
    protected final Lock routingTableWriteLock = routingTableLock.writeLock();

    /*** Link State ***/
    private static int[] LegacyLinkStateID = new int[0];
    private static LSA[] LSADatabase = new LSA[0];
    private LSA LSA;
    private int linkStateID = 0;

    /*** Identification ***/
    private final int IPAddress;
    private final UUID ID;
    private final int dim;
    private final BlockCoord location;
    private static int nextIP = 1;
    private static BitSet usedIPs = new BitSet();
    
    /*** Caches ***/
    private WeakReference<IWorldRouter> parent;

    public Router(UUID id, int dim, BlockCoord bc)
    {
        this.ID = id == null ? UUID.randomUUID() : id;
        this.dim = dim;
        this.location = bc;
        clearParentCache();
        this.LSA = new LSA();

        LSADatabasewriteLock.lock();

        this.IPAddress = claimIPAddress();
        if (LSADatabase.length <= IPAddress)
        {
            int newLength = (int) (IPAddress * 1.5) + 1;

            LSA[] SharedLSADatabase2 = new LSA[newLength];
            System.arraycopy(LSADatabase, 0, SharedLSADatabase2, 0, LSADatabase.length);
            LSADatabase = SharedLSADatabase2;

            int[] LSALegacyVersion2 = new int[newLength];
            System.arraycopy(LegacyLinkStateID, 0, LSALegacyVersion2, 0, LegacyLinkStateID.length);
            LegacyLinkStateID = LSALegacyVersion2;
        }
        LegacyLinkStateID[IPAddress] = 0;
        LSADatabase[IPAddress] = LSA;

        LSADatabasewriteLock.unlock();
    }

    public void update(IWorldRouter tickSource, boolean force)
    {
        buildParentCache(tickSource);
        if (force)
        {
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

    private boolean updateLSAIfNeeded()
    {
        boolean adjacentChanged = false;
        IWorldRouter wr = getParent();
        if (wr == null)
            return false;

        HashMap<Router, StartEndPath> newAdjacent;
        LSPathFinder finder = new LSPathFinder(wr, Configurator.maxDetectionCount, Configurator.maxDetectionLength);
        newAdjacent = finder.getResult();

        Iterator<Router> it = newAdjacent.keySet().iterator();
        while (it.hasNext())
        {
            Router r = it.next();
            if (r.getParent() == null)
                it.remove();
            else if (r.getParent().needsWork())
                return false;
        }

        if (adjacentLinks.size() != newAdjacent.size()) // Different number of connections
            adjacentChanged = true;

        else if (!adjacentChanged)
            for (Router r : adjacentLinks.keySet())
            {
                if (!newAdjacent.containsKey(r))
                { // Members missing
                    adjacentChanged = true;
                    break;
                }
            }
        else if (!adjacentChanged) // Edges different
            for (Entry<Router, StartEndPath> r : newAdjacent.entrySet())
            {
                StartEndPath oldEdge = adjacentLinks.get(r.getKey());
                if (oldEdge == null)
                {
                    adjacentChanged = true;
                    break;
                }

                StartEndPath newEdge = r.getValue();
                if (!newEdge.equals(oldEdge))
                {
                    adjacentChanged = true;
                    break;
                }
            }

        // Update local LSA
        if (adjacentChanged)
        {
            EnumSet<ForgeDirection> newExits = EnumSet.noneOf(ForgeDirection.class);
            for (Entry<Router, StartEndPath> r : newAdjacent.entrySet())
                newExits.add(ForgeDirection.getOrientation(r.getValue().dirToFirstHop));

            adjacentLinks = Collections.unmodifiableMap(newAdjacent);
            routedExits = newExits;

            HashMap<Router, Integer> neighboursWithCost = new HashMap<Router, Integer>();
            for (Entry<Router, StartEndPath> r : adjacentLinks.entrySet())
                neighboursWithCost.put(r.getKey(), r.getValue().distance);

            LSADatabasewriteLock.lock();
            LSA.neighbors = neighboursWithCost;
            LSADatabasewriteLock.unlock();
        }

        return adjacentChanged;
    }

    private void startLSAFloodfill()
    {
        BitSet prev = new BitSet(getEndOfIPPool());
        prev.set(IPAddress);

        for (Router r : adjacentLinks.keySet())
            r.LSAUpdateFloodfill(prev);

        prev.clear();
        prev.set(IPAddress);

        flagForRoutingUpdate();

        for (Router r : adjacentLinks.keySet())
            r.adjacentUpdateFloodfill(prev);
    }

    public void LSAUpdateFloodfill(BitSet prev)
    {
        if (prev.get(IPAddress))
            return;

        prev.set(IPAddress);

        updateLSAIfNeeded();
        // getParent().refreshState();

        for (Router r : adjacentLinks.keySet())
            r.LSAUpdateFloodfill(prev);
    }

    public void adjacentUpdateFloodfill(BitSet prev)
    {
        if (prev.get(IPAddress))
            return;

        prev.set(IPAddress);

        flagForRoutingUpdate();

        for (Router r : adjacentLinks.keySet())
            r.adjacentUpdateFloodfill(prev);
    }

    public void flagForRoutingUpdate()
    {
        linkStateID++;
    }

    public void decommission()
    {
        LSADatabasewriteLock.lock();
        if (IPAddress < LSADatabase.length)
            LSADatabase[IPAddress] = null;
        LSADatabasewriteLock.unlock();

        RouterServices.instance.removeRouter(IPAddress);
        
        clearParentCache();
        startLSAFloodfill();
        releaseIPAddress(IPAddress);
    }

    public int getLinkStateID()
    {
        return linkStateID;
    }

    public int getIPAddress()
    {
        return IPAddress;
    }

    public UUID getID()
    {
        return ID;
    }

    public List<ForgeDirection> getRouteTable()
    {
        refreshRouteTableIfNeeded(true);
        return routeTable;
    }

    public List<StartEndPath> getRoutesByCost()
    {
        refreshRouteTableIfNeeded(true);
        return routersByCost;
    }

    public ForgeDirection getExitDirection(int destination)
    {
        if (destination < 0)
            return ForgeDirection.UNKNOWN;

        if (!RouterServices.instance.routerExists(destination))
            return ForgeDirection.UNKNOWN;

        List<ForgeDirection> routeTable = getRouteTable();

        if (destination >= routeTable.size())
            return ForgeDirection.UNKNOWN;

        ForgeDirection dir = routeTable.get(destination);
        if (dir == null)
            return ForgeDirection.UNKNOWN;

        return dir;
    }

    public boolean canRouteTo(int destination)
    {
        if (destination < 0)
            return false;

        if (!RouterServices.instance.routerExists(destination))
            return false;

        List<ForgeDirection> routeTable = getRouteTable();

        if (destination >= routeTable.size())
            return false;

        return routeTable.get(destination) != null;
    }

    private void refreshRouteTableIfNeeded(boolean force)
    {
        if (linkStateID > LegacyLinkStateID[IPAddress])
            if (Configurator.routerUpdateThreadCount > 0 && !force)
                TableUpdateThread.add(this);
            else
                refreshRoutingTable(linkStateID);
    }

    public void refreshRoutingTable(int newVersion)
    {
        if (LegacyLinkStateID[IPAddress] >= newVersion)
            return;

        int sizeEstimate = getEndOfIPPool();
        if (sizeEstimate == 0)
            sizeEstimate = LSADatabase.length;

        /** Map of all routers in this network and its rudimentary path **/
        HashMap<Router, StartEndPath> tree2 = new HashMap<Router, StartEndPath>(sizeEstimate);
        /** List of all routers in this network ordered by cost **/
        ArrayList<StartEndPath> routersByCost2 = new ArrayList<StartEndPath>(sizeEstimate);
        /** Queue of all candidates that need checking **/
        PriorityQueue<StartEndPath> candidates2 = new PriorityQueue<StartEndPath>((int) Math.sqrt(sizeEstimate));

        // Start by adding our info.
        tree2.put(this, new StartEndPath(this, this, -1, 0));
        for (Router r : adjacentLinks.keySet())
        {
            StartEndPath l = adjacentLinks.get(r);
            candidates2.add(new StartEndPath(l.end, l.end, l.dirToFirstHop, l.distance));
        }
        routersByCost2.add(new StartEndPath(this, this, -1, 0));

        LSADatabasereadLock.lock();
        StartEndPath nextLowest = null;
        while ((nextLowest = candidates2.poll()) != null)
        {
            // We already approved this router. Keep skipping until we get a
            // fresh one.
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

            // Add all of the lowest's neighbors so they are checked later.
            LSA lsa = null;
            if (nextLowest.end.getIPAddress() < LSADatabase.length)
                lsa = LSADatabase[nextLowest.end.getIPAddress()];

            if (lsa != null)
                for (Router r : ((Map<Router, Integer>) lsa.neighbors.clone()).keySet())
                {
                    if (tree2.containsKey(r))
                        continue;

                    int newCost = nextLowest.distance + lsa.neighbors.get(r);
                    candidates2.add(new StartEndPath(lowestPath.end, r, lowestPath.dirToFirstHop, newCost));
                }

            // Approve this candidate
            nextLowest.start = lowestPath.start;
            tree2.put(nextLowest.end, nextLowest);
            routersByCost2.add(nextLowest);
        }
        LSADatabasereadLock.unlock();

        ArrayList<ForgeDirection> routeTable2 = new ArrayList<ForgeDirection>(getEndOfIPPool() + 1);
        while (getIPAddress() >= routeTable2.size())
            routeTable2.add(null);
        routeTable2.set(getIPAddress(), ForgeDirection.UNKNOWN);

        for (StartEndPath l : tree2.values())
        {
            Router firstHop = l.start;
            if (firstHop == null)
            {
                while (l.end.getIPAddress() >= routeTable2.size())
                    routeTable2.add(null);
                routeTable2.set(l.end.getIPAddress(), ForgeDirection.UNKNOWN);

                continue;
            }

            StartEndPath localOutPath = adjacentLinks.get(firstHop);
            if (localOutPath == null)
                continue;

            while (l.end.getIPAddress() >= routeTable2.size())
                routeTable2.add(null);
            routeTable2.set(l.end.getIPAddress(), ForgeDirection.getOrientation(localOutPath.dirToFirstHop));
        }

        // Set the new routing tables.
        routingTableWriteLock.lock();
        if (newVersion == linkStateID)
        {
            LSADatabasereadLock.lock();
            if (LegacyLinkStateID[IPAddress] < newVersion)
            {
                LegacyLinkStateID[IPAddress] = newVersion;
                routeTable = Collections.unmodifiableList(routeTable2);
                routersByCost = Collections.unmodifiableList(routersByCost2);
            }
            LSADatabasereadLock.unlock();
        }
        routingTableWriteLock.unlock();
    }

    public IWorldRouter getParent()
    {
        return parent == null ? null : parent.get();
    }
    
    public void clearParentCache()
    {
        if (parent != null)
            parent.clear();
        parent = null;
    }
    
    private void buildParentCache(IWorldRouter parent)
    {
        if (this.parent == null)
            this.parent = new WeakReference<IWorldRouter>(parent);
    }

    public World getWorld()
    {
        return DimensionManager.getWorld(dim);
    }

    private static int claimIPAddress()
    {
        int ip = usedIPs.nextClearBit(nextIP);
        nextIP = ip + 1;
        usedIPs.set(ip);
        return ip;
    }

    private static void releaseIPAddress(int ip)
    {
        usedIPs.clear(ip);
        if (ip < nextIP)
            nextIP = ip;
    }

    public static int getEndOfIPPool()
    {
        return usedIPs.size();
    }

    @Override
    public int hashCode()
    {
        return IPAddress;
    }

    public int getDim()
    {
        return dim;
    }

    public BlockCoord getLocation()
    {
        return location;
    }

    public boolean LSAConnectionExists(ForgeDirection dir)
    {
        return routedExits.contains(dir);
    }

    @Override
    public int compareTo(Router o)
    {
        return IPAddress - o.getIPAddress();
    }

    public static void reboot()
    {
        LSADatabasewriteLock.lock();
        LSADatabase = new LSA[0];
        LegacyLinkStateID = new int[0];
        LSADatabasewriteLock.unlock();
        usedIPs.clear();
        nextIP = 1;
    }

    private static class LSA
    {
        /** Map of [Linked Routers, Distance] **/
        public HashMap<Router, Integer> neighbors = new HashMap<Router, Integer>();
    }

    public static class StartEndPath implements Comparable<StartEndPath>
    {
        public int dirToFirstHop;
        public int distance;

        public Router start;
        public final Router end;

        public StartEndPath(Router start, Router end, int dir, int length)
        {
            this.start = start;
            this.end = end;

            this.dirToFirstHop = dir;
            this.distance = length;
        }

        @Override
        public boolean equals(Object o)
        {
            if (o instanceof StartEndPath)
            {
                StartEndPath p = (StartEndPath) o;
                return dirToFirstHop == p.dirToFirstHop && distance == p.distance;
            }
            return false;
        }

        @Override
        public int compareTo(StartEndPath o)
        {
            int c = distance - o.distance;
            if (c == 0)
                c = end.getIPAddress() - o.end.getIPAddress();
            return c;
        }
    }
}
