package mrtjp.projectred.transportation

import java.util.UUID
import java.util.concurrent.locks.ReentrantReadWriteLock
import java.util.{PriorityQueue => JPriorityQueue}
import mrtjp.projectred.core.Configurator
import mrtjp.projectred.core.utils.ItemKey
import mrtjp.projectred.transportation.SendPriority.SendPriority
import net.minecraftforge.common.ForgeDirection
import scala.collection.immutable.BitSet
import scala.collection.immutable.HashMap
import scala.collection.mutable

object RouterServices
{
    /** All registered routers **/
    private var routers = Array[Router]()
    /** Map of [ID, IP] for all registered routers **/
    private var UUIDTable = HashMap[UUID, Int]()

    def getRouter(id:Int) =
    {
        if (routers.isDefinedAt(id)) routers(id)
        else null
    }

    def getIPforUUID(id:UUID):Int =
    {
        if (id == null) return -1
        UUIDTable.getOrElse(id, -1)
    }

    def removeRouter(id:Int)
    {
        if (routers.isDefinedAt(id)) routers(id) = null
    }

    def getOrCreateRouter(uu:UUID, holder:IWorldRouter):Router =
    {
        routers synchronized
            {
                for (r <- routers) if (r != null && r.getParent == holder) return r
                val r = Router(uu, holder)

                val newLease = r.getIPAddress
                if (routers.length <= newLease)
                    while (routers.length <= (newLease*1.5).asInstanceOf[Int]+1) routers :+= null

                routers(newLease) = r
                UUIDTable += (r.getID -> r.getIPAddress)
                return r
            }
    }

    def routerExists(ip:Int) =
    {
        if (routers.isDefinedAt(ip)) routers(ip) != null
        else false
    }

    def reboot()
    {
        routers = Array[Router]()
        UUIDTable = HashMap[UUID, Int]()
    }
}

class LSA
{
    /** Map of [Linked Routers, Absolute Path] **/
    var neighbors = HashMap[Router, StartEndPath]()
}

class StartEndPath(var start:Router, var end:Router, var dirToFirstHop:Int, var distance:Int, val filters:Set[PathFilter]) extends Ordered[StartEndPath]
{
    def this(start:Router, end:Router, dirToFirstHop:Int, distance:Int, filter:PathFilter) = this(start, end, dirToFirstHop, distance, Set(filter))
    def this(start:Router, end:Router, dirToFirstHop:Int, distance:Int) = this(start, end, dirToFirstHop, distance, PathFilter.default)

    override def equals(other:Any) = other match
    {
        case that:StartEndPath =>
                dirToFirstHop == that.dirToFirstHop &&
                distance == that.distance &&
                filters == that.filters
        case _ => false
    }

    override def compare(that:StartEndPath) =
    {
        var c = distance-that.distance
        if (c == 0) c = end.getIPAddress-that.end.getIPAddress
        c
    }

    def allowItem(item:ItemKey) = filters.forall(f => f.filterExclude != f.filterItems.contains(item))
    def allowRouting = filters.forall(f => f.allowRouting)
    def allowBroadcast = filters.forall(f => f.allowBroadcast)
    def allowCrafting = filters.forall(f => f.allowCrafting)
    def allowController = filters.forall(f => f.allowController)

    val pathFlags = filters.foldLeft(0x7)((b, f) => f.pathFlags&b)
    val emptyFilter = !filters.exists(f => f != PathFilter.default)
}

object PathFilter
{
    val default = new PathFilter
    {
        override val filterExclude = true
        override val filterItems = Set[ItemKey]()

        override val allowRouting = true
        override val allowBroadcast = true
        override val allowCrafting = true
        override val allowController = true
    }
}

abstract class PathFilter
{
    val filterExclude:Boolean
    val filterItems:Set[ItemKey]

    val allowRouting:Boolean
    val allowBroadcast:Boolean
    val allowCrafting:Boolean
    val allowController:Boolean

    /**
     * 0x1 = can route to
     * 0x2 = can request from
     * 0x4 = can access controller from
     */
    def pathFlags =
        (if (allowRouting) 1 else 0) |
            (if (allowBroadcast||allowCrafting) 2 else 0) |
            (if (allowController) 4 else 0)

    override def equals(other:Any) = other match
    {
        case that:PathFilter =>
            filterExclude == that.filterExclude &&
                filterItems == that.filterItems &&
                allowRouting == that.allowRouting &&
                allowBroadcast == that.allowBroadcast &&
                allowCrafting == that.allowCrafting &&
                allowController == that.allowController
        case _ => false
    }
}

object Router
{
    /*** Locks for allowing thread-safe read-write access to router information ***/
    protected val LSADatabaseLock = new ReentrantReadWriteLock
    protected val LSADatabasereadLock = LSADatabaseLock.readLock
    protected val LSADatabasewriteLock = LSADatabaseLock.writeLock

    /*** Link State ***/
    private var LegacyLinkStateID = Array[Int]()
    private var LSADatabase = Array[LSA]()

    /*** Identification ***/
    private var nextIP = 1
    private var usedIPs = BitSet()

    private def claimIPAddress() =
    {
        val ip =
        {
            var i = nextIP
            while (usedIPs(i)) i+=1
            i
        }
        nextIP = ip+1
        usedIPs += ip
        ip
    }

    private def releaseIPAddress(ip:Int)
    {
        usedIPs -= ip
        if (ip < nextIP) nextIP = ip
    }

    def getEndOfIPPool = usedIPs.lastKey

    def reboot()
    {
        LSADatabasewriteLock.lock()
        LSADatabase = new Array[LSA](0)
        LegacyLinkStateID = new Array[Int](0)
        LSADatabasewriteLock.unlock()

        usedIPs = BitSet.empty
        nextIP = 1
    }

    def apply(ID:UUID, parent:IWorldRouter) =
    {
        /*** Construct ***/
        val ID2 = if (ID == null) UUID.randomUUID else ID
        val r = new Router(ID2, parent)
        LSADatabasewriteLock.lock()
        val ip = Router.claimIPAddress()
        r.IPAddress = ip
        if (LSADatabase.length <= ip)
        {
            val newLength = (ip*1.5).asInstanceOf[Int]+1
            while (LSADatabase.length <= newLength) LSADatabase :+= null
            while (LegacyLinkStateID.length <= newLength) LegacyLinkStateID :+= -1
        }
        LegacyLinkStateID(ip) = 0
        LSADatabase(ip) = r.LSA
        LSADatabasewriteLock.unlock()
        r
    }
}

class Router(ID:UUID, parent:IWorldRouter) extends Ordered[Router]
{
    /** List of [ForgeDirection] indexed by IP for all routers in this network **/
    private var routeTable = Vector[Vector[StartEndPath]]()
    /** List of [StartEndPath] sorted by distance, closest one having the lowest index **/
    private var routersByCost = Vector[StartEndPath]()
    /** Set of known exits from here. Used to determine render color of a side **/
    private var routedExits = java.util.EnumSet.noneOf(classOf[ForgeDirection]) //TODO seems a little chunky, how about a bitset?
    /** Link State for all nodes that branch from this one **/
    private var adjacentLinks = HashMap[Router, StartEndPath]()

    /*** Locks for allowing thread-safe read-write access to router information ***/
    protected val routingTableLock = new ReentrantReadWriteLock
    protected val routingTableReadLock = routingTableLock.readLock
    protected val routingTableWriteLock = routingTableLock.writeLock

    /*** Link State ***/
    private val LSA = new LSA
    private var linkStateID = 0

    /*** Identification ***/
    private var IPAddress = 0
    private var decommissioned = false

    def update(force:Boolean)
    {
        if (force)
        {
            if (updateLSAIfNeeded()) startLSAFloodfill()
            refreshRouteTableIfNeeded(false)
            val r = getParent
            if (r != null) r.refreshState
            return
        }
        if (Configurator.routerUpdateThreadCount > 0) refreshRouteTableIfNeeded(false)
    }

    private def updateLSAIfNeeded():Boolean =
    {
        var adjacentChanged = false
        val wr = getParent

        if (wr == null) return false

        val finder = new LSPathFinder(wr, Configurator.maxDetectionCount, Configurator.maxDetectionLength)
        val newAdjacent = finder.getResult.filter(kv => kv._1.isLoaded)

        adjacentChanged = adjacentLinks != newAdjacent

        if (adjacentChanged)
        {
            val newExits = java.util.EnumSet.noneOf(classOf[ForgeDirection])
            for ((k,v) <- newAdjacent) newExits.add(ForgeDirection.getOrientation(v.dirToFirstHop))
            adjacentLinks = newAdjacent
            routedExits = newExits

            var neighboursWithCost = HashMap[Router, StartEndPath]()
            for ((k,v) <- adjacentLinks) neighboursWithCost += (k -> v)

            Router.LSADatabasewriteLock.lock()
            LSA.neighbors = neighboursWithCost
            Router.LSADatabasewriteLock.unlock()
        }
        adjacentChanged
    }

    private def startLSAFloodfill()
    {
        val prev = mutable.BitSet(Router.getEndOfIPPool)
        prev += IPAddress
        for (r <- adjacentLinks.keySet) r.LSAUpdateFloodfill(prev)
        prev.clear()
        prev += IPAddress

        flagForRoutingUpdate()
        for (r <- adjacentLinks.keySet) r.adjacentUpdateFloodfill(prev)
    }

    def LSAUpdateFloodfill(prev:mutable.BitSet)
    {
        if (prev(IPAddress)) return
        prev += IPAddress
        updateLSAIfNeeded()
        for (r <- adjacentLinks.keySet) if (r.isLoaded) r.LSAUpdateFloodfill(prev)
    }

    def adjacentUpdateFloodfill(prev:mutable.BitSet)
    {
        if (prev(IPAddress)) return
        prev += IPAddress
        flagForRoutingUpdate()
        for (r <- adjacentLinks.keySet) if (r.isLoaded) r.adjacentUpdateFloodfill(prev)
    }

    def flagForRoutingUpdate()
    {
        linkStateID += 1
    }

    def decommission()
    {
        Router.LSADatabasewriteLock.lock()
        if (Router.LSADatabase.isDefinedAt(IPAddress)) Router.LSADatabase(IPAddress) = null
        Router.LSADatabasewriteLock.unlock()

        RouterServices.removeRouter(IPAddress)
        decommissioned = true
        startLSAFloodfill()
        Router.releaseIPAddress(IPAddress)
    }

    def getLinkStateID = linkStateID

    def getIPAddress = IPAddress

    def getID = ID

    def getRouteTable =
    {
        refreshRouteTableIfNeeded(true)
        routeTable
    }

    def getRoutesByCost =
    {
        refreshRouteTableIfNeeded(true)
        routersByCost.filter(p => p != null && p.end.isLoaded)
    }

    def getFilteredRoutesByCost(f:StartEndPath => Boolean) =
    {
        refreshRouteTableIfNeeded(true)
        routersByCost.filter(p => p != null && p.end.isLoaded && f(p))
    }

    def canRouteTo(destination:Int, item:ItemKey, priority:SendPriority) = pathTo(destination, item, priority) != null
    def getDirection(destination:Int, item:ItemKey, priority:SendPriority) = pathTo(destination, item, priority) match
    {
        case path:StartEndPath => ForgeDirection.getOrientation(path.dirToFirstHop)
        case _ => ForgeDirection.UNKNOWN
    }

    private def pathTo(destination:Int, item:ItemKey, priority:SendPriority):StartEndPath =
    {
        val rt = getRouteTable
        if (rt.isDefinedAt(destination) && RouterServices.routerExists(destination))
        {
            val paths = rt(destination)
            if (paths != null)
                for (path <- paths)
                    if (path.allowRouting && (!priority.active || path.allowBroadcast || path.allowCrafting) && path.allowItem(item))
                        return path
        }
        null
    }

    private def refreshRouteTableIfNeeded(force:Boolean)
    {
        if (linkStateID > Router.LegacyLinkStateID(IPAddress))
        {
            if (Configurator.routerUpdateThreadCount > 0 && !force) TableUpdateThread.add(this)
            else refreshRoutingTable(linkStateID)
        }
    }

    def refreshRoutingTable(newVer:Int)
    {
        if (Router.LegacyLinkStateID(IPAddress) >= newVer) return
        val sizeEstimate = Router.getEndOfIPPool match
        {
            case 0 => Router.LSADatabase.length
            case notZero => notZero
        }

        val localPath = new StartEndPath(this, this, -1, 0)
        /** List of all paths in this network ordered by cost **/
        var routersByCost2 = Vector[StartEndPath](localPath)
        /** Queue of all candidates that need checking **/
        val candidates2 = new JPriorityQueue[StartEndPath](Math.sqrt(sizeEstimate).asInstanceOf[Int])//scala PQ is not working ?!

        /** Previously found properties that are checked before adding a new path **/
        var closedFilters = new Array[Vector[Set[PathFilter]]](Router.getEndOfIPPool+1)
        def ensureClosed(size:Int){while (closedFilters.length <= size) closedFilters :+= null}

        // Start by adding our info.
        for ((k,v) <- adjacentLinks) candidates2.add(new StartEndPath(v.end, v.end, v.dirToFirstHop, v.distance, v.filters))
        closedFilters(getIPAddress) = Vector(Set[PathFilter](PathFilter.default))

        import mrtjp.projectred.core.utils.LabelBreaks._
        Router.LSADatabasereadLock.lock()
        while (!candidates2.isEmpty) label
        {
            val dequeue = candidates2.poll()
            val deqIP = dequeue.end.getIPAddress
            ensureClosed(deqIP)

            //Skip this one UNLESS the filters on this differs from previously discovered paths to here.
            val filtSetsClosed = closedFilters(deqIP)
            if (filtSetsClosed != null) for (filtsClosed <- filtSetsClosed)
                if (filtsClosed.subsetOf(dequeue.filters)) break()

            // Add all of the lowest's neighbors so they are checked later.
            val lsa = deqIP match
            {
                case ip if Router.LSADatabase.isDefinedAt(ip) => Router.LSADatabase(ip)
                case _ => null
            }

            if (lsa != null) for ((k,v) <- lsa.neighbors) if ((dequeue.pathFlags&v.pathFlags) != 0)//if we can do something with this route
                candidates2.add(new StartEndPath(dequeue.start, k, dequeue.dirToFirstHop, dequeue.distance+v.distance,
                    dequeue.filters ++ v.filters))

            // Approve this candidate
            if ((dequeue.pathFlags&0x3) != 0) routersByCost2 :+= dequeue //if can route to or request from its a possibe routeByCost
            closedFilters(deqIP) = if (filtSetsClosed != null) filtSetsClosed ++ Vector(dequeue.filters) else Vector(dequeue.filters)
        }
        Router.LSADatabasereadLock.unlock()

        var routeTable2 = new Array[Vector[StartEndPath]](Router.getEndOfIPPool+1)
        def ensureRT2(size:Int) {while (routeTable2.length <= size) routeTable2 :+= null}

        ensureRT2(getIPAddress)
        routeTable2(getIPAddress) = Vector(new StartEndPath(this, this, -1, 0)) //consider ourselves for logistic path

        for (p <- routersByCost2) if (p != null) label
        {
            val firstHop = p.start
            val rootHop = adjacentLinks.getOrElse(firstHop, null)

            if (rootHop == null) break()

            p.start = this
            p.dirToFirstHop = rootHop.dirToFirstHop

            val endIP = p.end.getIPAddress
            ensureRT2(endIP)
            val prev = routeTable2(endIP)
            routeTable2(endIP) = if (prev != null) prev :+ p else Vector(p)
        }

        // Set the new routing tables.
        routingTableWriteLock.lock()
        if (newVer == linkStateID)
        {
            Router.LSADatabasereadLock.lock()
            if (Router.LegacyLinkStateID(IPAddress) < newVer)
            {
                Router.LegacyLinkStateID(IPAddress) = newVer
                routeTable = routeTable2.toVector
                routersByCost = routersByCost2.toVector
            }
            Router.LSADatabasereadLock.unlock()
        }
        routingTableWriteLock.unlock()
    }

    def getParent = parent

    def isLoaded:Boolean =
    {
        if (decommissioned) return false
        val parent = getParent
        if (getParent == null) return false
        if (getParent.needsWork) return false
        if (parent.getContainer.tile == null || parent.getContainer.tile.isInvalid) return false
        true
    }

    def LSAConnectionExists(dir:ForgeDirection) = routedExits.contains(dir)

    override def hashCode = IPAddress

    override def compare(that:Router) = IPAddress-that.getIPAddress
}