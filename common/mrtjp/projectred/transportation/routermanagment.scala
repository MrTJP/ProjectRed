package mrtjp.projectred.transportation

import java.util.UUID
import java.util.concurrent.locks.ReentrantReadWriteLock
import mrtjp.projectred.core.Configurator
import net.minecraftforge.common.ForgeDirection
import scala.collection.immutable.BitSet
import scala.collection.immutable.HashMap
import scala.collection.{JavaConversions, mutable}

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
    /** Map of [Linked Routers, Distance] **/
    var neighbors = HashMap[Router, Int]()
}

class StartEndPath(var start:Router, var end:Router, var dirToFirstHop:Int, var distance:Int) extends Ordered[StartEndPath]
{
    override def equals(other:Any) = other match
    {
        case that:StartEndPath =>
                dirToFirstHop == that.dirToFirstHop &&
                distance == that.distance
        case _ => false
    }

    override def compare(that:StartEndPath) =
    {
        var c = distance-that.distance
        if (c == 0) c = end.getIPAddress-that.end.getIPAddress
        c
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

    def getEndOfIPPool = usedIPs.size

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
    private var routeTable = Array[ForgeDirection]()
    /** List of [StartEndPath] sorted by distance, closest one having the lowest index **/
    private var routersByCost = Array[StartEndPath]()
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
        var adjacentChanged:Boolean = false
        val wr:IWorldRouter = getParent

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

            var neighboursWithCost = HashMap[Router, Int]()
            for ((k,v) <- adjacentLinks) neighboursWithCost += (k -> v.distance)

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

    def getExitDirection(destination:Int):ForgeDirection =
    {
        val rt = getRouteTable
        if (rt.isDefinedAt(destination) && RouterServices.routerExists(destination))
        {
            val dir = rt(destination)
            if (dir == null) return ForgeDirection.UNKNOWN else return dir
        }

        ForgeDirection.UNKNOWN
    }

    def canRouteTo(destination:Int):Boolean =
    {
        val rt = getRouteTable
        rt.isDefinedAt(destination) &&
            RouterServices.routerExists(destination) &&
            rt(destination) != null
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

        /** Map of all routers in this network and its rudimentary path **/
        var tree2 = new HashMap[Router, StartEndPath]()
        /** List of all routers in this network ordered by cost **/
        var routersByCost2 = new Array[StartEndPath](sizeEstimate)
        /** Queue of all candidates that need checking **/
        var candidates2 = new mutable.PriorityQueue[StartEndPath]()

        // Start by adding our info.
        tree2 += (this -> new StartEndPath(this, this, -1, 0))
        for ((k,v) <- adjacentLinks) candidates2 += new StartEndPath(v.end, v.end, v.dirToFirstHop, v.distance)
        routersByCost2 :+= new StartEndPath(this, this, -1, 0)

        import mrtjp.projectred.core.utils.LabelBreaks._
        Router.LSADatabasereadLock.lock()
        while (!candidates2.isEmpty) label
        {
            var dequeue = candidates2.dequeue()

            // We already approved this router. Keep skipping until we get a fresh one.
            while (!candidates2.isEmpty && tree2.keySet.contains(dequeue.end)) dequeue = candidates2.dequeue()
            if (tree2.keySet.contains(dequeue.end)) break()

            // This is the lowest path so far to here.
            // If its null, then we are the start of the path to this router.
            val low = tree2.getOrElse(dequeue.start, dequeue)

            // Add all of the lowest's neighbors so they are checked later.
            val lsa =
            {
                val ip = dequeue.end.getIPAddress
                if (Router.LSADatabase.isDefinedAt(ip)) Router.LSADatabase(ip)
                else null
            }
            if (lsa != null) for ((k,v) <- lsa.neighbors) if (!tree2.contains(k))
                candidates2 += new StartEndPath(low.end, k, low.dirToFirstHop, dequeue.distance+v)

            // Approve this candidate
            dequeue.start = low.start
            tree2 += (dequeue.end -> dequeue)
            routersByCost2 :+= dequeue
        }
        Router.LSADatabasereadLock.unlock()

        var routeTable2 = new Array[ForgeDirection](Router.getEndOfIPPool+1)
        while (getIPAddress >= routeTable2.length) routeTable2 :+= null
        routeTable2(getIPAddress) = ForgeDirection.UNKNOWN //consider ourselves for logistic path

        for (p <- tree2.values) label
        {
            val firstHop = p.start
            if (firstHop == null)
            {
                while (p.end.getIPAddress >= routeTable2.length) routeTable2 :+= null
                routeTable2(p.end.getIPAddress) = ForgeDirection.UNKNOWN
                break()
            }

            val localOutPath = adjacentLinks.getOrElse(firstHop, null)
            if (localOutPath == null) break()

            while (p.end.getIPAddress >= routeTable2.length) routeTable2 :+= null
            routeTable2(p.end.getIPAddress) = ForgeDirection.UNKNOWN
            routeTable2(p.end.getIPAddress) = ForgeDirection.getOrientation(localOutPath.dirToFirstHop)
        }

        // Set the new routing tables.
        routingTableWriteLock.lock()
        if (newVer == linkStateID)
        {
            Router.LSADatabasereadLock.lock()
            if (Router.LegacyLinkStateID(IPAddress) < newVer)
            {
                Router.LegacyLinkStateID(IPAddress) = newVer
                routeTable = routeTable2
                routersByCost = routersByCost2
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