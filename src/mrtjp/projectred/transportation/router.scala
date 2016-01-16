package mrtjp.projectred.transportation

import java.util.concurrent.locks.ReentrantReadWriteLock
import java.util.{PriorityQueue => JPriorityQueue, UUID}

import mrtjp.core.item.ItemKey
import mrtjp.projectred.core.Configurator
import mrtjp.projectred.transportation.Priorities.NetworkPriority

import scala.collection.immutable.{BitSet, HashMap}
import scala.collection.mutable.{BitSet => MBitSet}

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
        Router.reboot()
    }
}

class LSA
{
    /** Vector of [StartEndPath] of all neighboring pipes **/
    var neighbors = Vector[StartEndPath]()
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
    /** Vector, indexed by IP, of Vector of all paths to said IP ordered by distance **/
    private var routeTable = Vector[Vector[StartEndPath]]()
    /** Vector of [StartEndPath] sorted by distance, closest one having the lowest index **/
    private var routersByCost = Vector[StartEndPath]()

    /** Mask of known exits from here. Used to determine render color. **/
    private var routedExits = 0
    /** Link State for all nodes that branch from this one **/
    private var adjacentLinks = Vector[StartEndPath]()

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
            if (r != null) r.refreshState()
            return
        }
        if (Configurator.routerUpdateThreadCount > 0) refreshRouteTableIfNeeded(false)
    }

    private def updateLSAIfNeeded():Boolean =
    {
        var adjacentChanged = false
        if (!isLoaded) return false

        LSPathFinder.clear()
        LSPathFinder.start = parent
        val newAdjacent = LSPathFinder.result().filter(_.end.isLoaded)
        LSPathFinder.clear()

        adjacentChanged = adjacentLinks != newAdjacent

        if (adjacentChanged)
        {
            adjacentLinks = newAdjacent
            routedExits = newAdjacent.foldLeft(0)((b, p) => (b|1<<p.hopDir)&0x3F)

            Router.LSADatabasewriteLock.lock()
            LSA.neighbors = adjacentLinks.toVector //add to database so another thread can make the route table.
            Router.LSADatabasewriteLock.unlock()
        }
        adjacentChanged
    }

    private def startLSAFloodfill()
    {
        val prev = MBitSet(Router.getEndOfIPPool)
        prev += IPAddress
        for (p <- adjacentLinks) p.end.LSAUpdateFloodfill(prev)
        prev.clear()
        prev += IPAddress

        flagForRoutingUpdate()
        for (p <- adjacentLinks) p.end.adjacentUpdateFloodfill(prev)
    }

    def LSAUpdateFloodfill(prev:MBitSet)
    {
        if (prev(IPAddress)) return
        prev += IPAddress
        updateLSAIfNeeded()
        for (p <- adjacentLinks) p.end.LSAUpdateFloodfill(prev)
    }

    def adjacentUpdateFloodfill(prev:MBitSet)
    {
        if (prev(IPAddress)) return
        prev += IPAddress
        flagForRoutingUpdate()
        for (p <- adjacentLinks) p.end.adjacentUpdateFloodfill(prev)
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

    def isInNetwork(destination:Int):Boolean =
    {
        val rt = getRouteTable
        if (rt.isDefinedAt(destination) && RouterServices.routerExists(destination))
        {
            val paths = rt(destination)
            if (paths != null) return true
        }
        false
    }

    def canRouteTo(destination:Int, item:ItemKey, priority:NetworkPriority) = pathTo(destination, item, priority) != null
    def getDirection(destination:Int, item:ItemKey, priority:NetworkPriority) = pathTo(destination, item, priority) match
    {
        case path:StartEndPath => path.hopDir
        case _ => 6
    }

    private def pathTo(destination:Int, item:ItemKey, priority:NetworkPriority):StartEndPath =
    {
        val rt = getRouteTable
        if (rt.isDefinedAt(destination) && RouterServices.routerExists(destination))
        {
            val paths = rt(destination)
            if (paths != null)
                for (path <- paths) if (path.flagRouteTo)
                    if (priority.isPathUsable(path) && path.allowItem(item))
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

        /** Vector of all paths in this network ordered by cost **/
        var routersByCost2 = Vector[StartEndPath](new StartEndPath(this, this, 6, 0))
        /** Queue of all candidates that need checking **/
        val candidates2 = new JPriorityQueue[StartEndPath](Math.sqrt(sizeEstimate).asInstanceOf[Int])//scala PQ is not working ?!

        /** Previously found properties that are checked before adding a new path **/
        var closedFilters = new Array[Vector[Set[PathFilter]]](Router.getEndOfIPPool+1)
        def ensureClosed(size:Int){while (closedFilters.length <= size) closedFilters :+= null}

        // Start by adding our info.
        for (p <- adjacentLinks) candidates2.add(p.createStartPoint)
        closedFilters(getIPAddress) = Vector(Set[PathFilter](PathFilter.default))

        val a, b = new scala.util.control.Breaks
        Router.LSADatabasereadLock.lock()
        while (!candidates2.isEmpty) a.breakable
        {
            val dequeue = candidates2.poll()
            val deqIP = dequeue.end.getIPAddress
            ensureClosed(deqIP)
            val pflags = dequeue.pathFlags

            //Skip if we have found path with identical filters
            val filtSetsClosed = closedFilters(deqIP)
            if (filtSetsClosed != null) for (filtsClosed <- filtSetsClosed)
                if (filtsClosed.subsetOf(dequeue.filters)) a.break() //dequeue's filters contain all closed filters

            // Add all of the lowest's neighbors so they are checked later.
            val lsa = deqIP match
            {
                case ip if Router.LSADatabase.isDefinedAt(ip) => Router.LSADatabase(ip)
                case _ => null
            }

            if (lsa != null)
            {
                for (p <- lsa.neighbors) if ((pflags&p.pathFlags) != 0) candidates2.add(dequeue --> p)
            }

            //Approve this candidate
            if ((pflags&0x3) != 0) routersByCost2 :+= dequeue //if can route to or request from its a possibe routeByCost
            closedFilters(deqIP) = if (filtSetsClosed != null) filtSetsClosed :+ dequeue.filters else Vector(dequeue.filters)
        }
        Router.LSADatabasereadLock.unlock()

        var routeTable2 = new Array[Vector[StartEndPath]](Router.getEndOfIPPool+1)
        def ensureRT2(size:Int) {while (routeTable2.length <= size) routeTable2 :+= null}

        ensureRT2(getIPAddress)
        routeTable2(getIPAddress) = Vector(new StartEndPath(this, this, 6, 0)) //consider ourselves for logistic path

        for (p <- routersByCost2) if (p != null) b.breakable
        {
            val firstHop = p.start
            val rootHop = adjacentLinks.find(_.end == firstHop).orNull

            if (rootHop == null) b.break()

            p.start = this
            p.hopDir = rootHop.hopDir

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
                routersByCost = routersByCost2
            }
            Router.LSADatabasereadLock.unlock()
        }
        routingTableWriteLock.unlock()
    }

    def getParent = parent

    def isLoaded =
    {
        if (decommissioned ||
            parent == null ||
            parent.needsWork ||
            parent.getContainer.tile == null ||
            parent.getContainer.tile.isInvalid) false
        else true
    }

    def LSAConnectionExists(dir:Int) = (routedExits&1<<dir) != 0

    override def hashCode = IPAddress

    override def compare(that:Router) = IPAddress-that.getIPAddress

    override def toString = "Router(["+IPAddress+"] "+ID+")"
}