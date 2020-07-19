package mrtjp.projectred.transportation

import java.util.concurrent.PriorityBlockingQueue
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

    def getOrCreateRouter(uu:UUID, holder:IRouterContainer):Router =
    {
        routers synchronized {
            for (r <- routers) if (r != null && r.getContainer == holder) return r
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
        val ip = {
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

    def apply(ID:UUID, parent:IRouterContainer) =
    {
        /*** Construct ***/
        val ID2 = if (ID == null) UUID.randomUUID else ID
        val ip = Router.claimIPAddress()
        val r = new Router(ID2, ip, parent)
        LSADatabasewriteLock.lock()
        if (LSADatabase.length <= ip) {
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

class Router(ID:UUID, IPAddress:Int, parent:IRouterContainer) extends Ordered[Router]
{
    private var decommisioned = false

    /** Vector, indexed by IP, of Vector of all paths to said IP ordered by distance **/
    private var routeTable = Vector[Vector[StartEndPath]]()
    /** Vector of [StartEndPath] sorted by distance, closest one having the lowest index **/
    private var routersByCost = Vector[StartEndPath]()

    /** Mask of known exits from here. Used to determine render color. **/
    private var routedExits = 0
    /** Link State for all nodes that branch from this one **/
    private var adjacentLinks = Vector[StartEndPath]()

    /*** Locks for allowing thread-safe read-write access to route table information ***/
    protected val routingTableLock = new ReentrantReadWriteLock
    protected val routingTableReadLock = routingTableLock.readLock
    protected val routingTableWriteLock = routingTableLock.writeLock

    /*** Link State ***/
    private val LSA = new LSA
    private var linkStateID = 0

    def getID = ID
    def getIPAddress = IPAddress
    def getLinkStateID = linkStateID
    def getContainer = parent

    def update(time:Long)
    {
        if ((time%Configurator.detectionFrequency) == (getIPAddress%Configurator.detectionFrequency)) {
            if (updateLSAIfNeeded()) startLSAFloodfill()
            refreshRouteTableIfNeeded(false)
            val r = getContainer
            if (r != null) r.refreshState(routedExits)
            return
        }
        if (Configurator.routerUpdateThreadCount > 0) refreshRouteTableIfNeeded(false)
    }

    private def updateLSAIfNeeded():Boolean =
    {
        var adjacentChanged = false
        if (!isLoaded) return false

        val newAdjacent = parent.searchForLinks.filter(_.end.isLoaded)
        adjacentChanged = adjacentLinks != newAdjacent

        if (adjacentChanged) {
            adjacentLinks = newAdjacent
            routedExits = newAdjacent.foldLeft(0)((b, p) => (b|1<<p.hopDir)&0x3F)

            Router.LSADatabasewriteLock.lock()
            LSA.neighbors = adjacentLinks //add to database so another thread can make the route table.
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

    private def LSAUpdateFloodfill(prev:MBitSet)
    {
        if (prev(IPAddress)) return
        prev += IPAddress
        updateLSAIfNeeded()
        for (p <- adjacentLinks) p.end.LSAUpdateFloodfill(prev)
    }

    private def adjacentUpdateFloodfill(prev:MBitSet)
    {
        if (prev(IPAddress)) return
        prev += IPAddress
        flagForRoutingUpdate()
        for (p <- adjacentLinks) p.end.adjacentUpdateFloodfill(prev)
    }

    private def flagForRoutingUpdate()
    {
        linkStateID += 1
    }

    private def pathTo(destination:Int, item:ItemKey, priority:NetworkPriority):StartEndPath =
    {
        val rt = getRouteTable
        if (rt.isDefinedAt(destination) && RouterServices.routerExists(destination)) {
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
        if (linkStateID > Router.LegacyLinkStateID(IPAddress)) {
            if (Configurator.routerUpdateThreadCount > 0 && !force) TableUpdateThread.add(this)
            else refreshRoutingTable(linkStateID)
        }
    }

    private[transportation] def refreshRoutingTable(newVer:Int)
    {
        if (Router.LegacyLinkStateID(IPAddress) >= newVer) return
        val sizeEstimate = Router.getEndOfIPPool match {
            case 0 => Router.LSADatabase.length
            case notZero => notZero
        }

        /** Vector of all paths in this network ordered by cost **/
        var routersByCost2 = Vector[StartEndPath](new StartEndPath(this, this, 6, 0))
        /** Queue of all paths that need checking **/
        val openPaths = new JPriorityQueue[StartEndPath](math.sqrt(sizeEstimate).toInt)//scala PQ is not working ?!

        /** Filters of previously found paths **/
        var closedFilters = new Array[Vector[Set[PathFilter]]](Router.getEndOfIPPool+1)
        def ensureClosed(size:Int){while (closedFilters.length <= size) closedFilters :+= null}

        //Start by adding our info.
        for (p <- adjacentLinks) openPaths.add(p)
        closedFilters(getIPAddress) = Vector(Set[PathFilter](PathFilter.default))

        Router.LSADatabasereadLock.lock()
        import scala.util.control.Breaks._
        while (!openPaths.isEmpty) breakable {
            val dequeue = openPaths.poll()
            val deqIP = dequeue.end.getIPAddress
            ensureClosed(deqIP)
            val pflags = dequeue.pathFlags

            //Skip if we have already found a path with identical filters
            val filtSetsClosed = closedFilters(deqIP)
            if (filtSetsClosed != null) for (filtsClosed <- filtSetsClosed)
                if (filtsClosed.subsetOf(dequeue.filters)) break() //dequeue's filters contain all closed filters

            //Queue all of the neighbors of the end of this path for searching
            val lsa = deqIP match {
                case ip if Router.LSADatabase.isDefinedAt(ip) => Router.LSADatabase(ip)
                case _ => null
            }

            if (lsa != null) {
                for (p <- lsa.neighbors) if ((pflags&p.pathFlags) != 0) openPaths.add(dequeue --> p)
            }

            //Approve this candidate
            if ((pflags&0x3) != 0) routersByCost2 :+= dequeue //if we can get to or come from using this path its a possibe route
            closedFilters(deqIP) = if (filtSetsClosed != null) filtSetsClosed :+ dequeue.filters else Vector(dequeue.filters)
        }
        Router.LSADatabasereadLock.unlock()

        var routeTable2 = new Array[Vector[StartEndPath]](Router.getEndOfIPPool+1)
        def ensureRT2(size:Int) {while (routeTable2.length <= size) routeTable2 :+= null}

        for (p <- routersByCost2) {
            val endIP = p.end.getIPAddress
            ensureRT2(endIP)
            val prev = routeTable2(endIP)
            routeTable2(endIP) = if (prev != null) prev :+ p else Vector(p)
        }

        // Set the new routing tables.
        routingTableWriteLock.lock()
        if (newVer == linkStateID) {
            Router.LSADatabasereadLock.lock()
            if (Router.LegacyLinkStateID(IPAddress) < newVer) {
                Router.LegacyLinkStateID(IPAddress) = newVer
                routeTable = routeTable2.toVector
                routersByCost = routersByCost2
            }
            Router.LSADatabasereadLock.unlock()
        }
        routingTableWriteLock.unlock()
    }

    def isLoaded =
    {
        if (decommisioned ||
            parent == null ||
            parent.getPipe.tile == null ||
            parent.getPipe.tile.isInvalid) false
        else true
    }

    override def hashCode = IPAddress

    override def compare(that:Router) = IPAddress-that.getIPAddress

    override def toString = "Router(["+IPAddress+"] "+ID+")"

    def decommision()
    {
        Router.LSADatabasewriteLock.lock()
        if (Router.LSADatabase.isDefinedAt(IPAddress)) Router.LSADatabase(IPAddress) = null
        Router.LSADatabasewriteLock.unlock()

        RouterServices.removeRouter(IPAddress)
        decommisioned = true
        startLSAFloodfill()
        Router.releaseIPAddress(IPAddress)
    }

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

    def resolvePayload(r:NetworkPayload):PayloadResolution =
    {
        r.refreshIP()

        if (r.destinationIP <= 0 || r.hasArrived) { //route unrouted payloads
            r.resetTrip()

            LogisticPathFinder.clear()
            LogisticPathFinder.start = this
            LogisticPathFinder.payload = r.payload.key
            val result = LogisticPathFinder.result()
            LogisticPathFinder.clear()

            if (result != null) {
                r.setDestination(result.responder, result.priority)
                if (r.destinationUUID == getID)
                    return RecievePayload()

                val path = pathTo(r.destinationIP, r.payload.key, r.netPriority)
                if (path != null) {
                    return RoutePayload(path.hopDir)
                }
            }
        }

        if (r.destinationIP > 0 && r.destinationUUID == getID) { //accept if they are for this router
            r.hasArrived = true
            return RecievePayload()
        }

        if (r.destinationUUID != getID) { //relay if they are not
            val path = pathTo(r.destinationIP, r.payload.key, r.netPriority)
            if (path != null)
                return RelayPayload(path.hopDir)
        }

        UnresolvedPayload()
    }
}

object TableUpdateThread
{
    private val updateCalls = new PriorityBlockingQueue[RouteLayerUpdater]()
    private var average = 0L

    val avgSync = new AnyRef

    def add(r:Router)
    {
        updateCalls.add(new RouteLayerUpdater(r))
    }

    def remove(run:Runnable) = updateCalls.remove(run)

    def size = updateCalls.size

    def getAverage = avgSync synchronized average
}

class TableUpdateThread(i:Int) extends Thread("PR RoutingThread #"+i)
{
    setDaemon(true)
    setPriority(Thread.NORM_PRIORITY)
    start()

    override def run()
    {
        import TableUpdateThread._

        var job:RouteLayerUpdater = null
        try {
            while ({job = updateCalls.take; job} != null) {
                val starttime = System.nanoTime
                job.run()
                val took = System.nanoTime-starttime

                TableUpdateThread.avgSync synchronized {
                    if (TableUpdateThread.average == 0) TableUpdateThread.average = took
                    else TableUpdateThread.average = (TableUpdateThread.average*999L+took)/1000L
                }
            }
        }
        catch {case e:InterruptedException =>}
    }
}

private[this] class RouteLayerUpdater(val router:Router) extends Runnable with Ordered[RouteLayerUpdater]
{
    private val newVersion = router.getLinkStateID
    private var complete = false

    def run()
    {
        if (complete) return
        try {
            if (router.getContainer == null) {
                var i = 0
                while (i < 10 && !router.isLoaded) {
                    Thread.sleep(10)
                    i += 1
                }
            }
            if (!router.isLoaded) return
            router.refreshRoutingTable(newVersion)
        }
        catch {case e:Exception => e.printStackTrace()}
        complete = true
    }

    override def compare(that:RouteLayerUpdater) =
    {
        var c = 0
        if (that.newVersion <= 0) c = newVersion-that.newVersion
        if (c == 0) c = router.getIPAddress-that.router.getIPAddress
        if (c == 0) c = that.newVersion-newVersion
        c
    }
}