package mrtjp.projectred.transportation

import codechicken.lib.vec.BlockCoord
import mrtjp.projectred.api.ISpecialLinkState
import mrtjp.projectred.core.libmc.{ItemKey, ItemQueue, PRLib}
import mrtjp.projectred.core.{AStar, PathNode}
import mrtjp.projectred.transportation.SendPriority.SendPriority
import net.minecraft.tileentity.TileEntity
import net.minecraft.world.World

import scala.collection.immutable.BitSet

object LSPathFinder
{
    private var registeredLSTypes = List[ISpecialLinkState]()

    def register(link:ISpecialLinkState)
    {
        registeredLSTypes :+= link
    }

    def getLinkState(tile:TileEntity):ISpecialLinkState =
    {
        for (l <- registeredLSTypes) if (l.matches(tile)) return l
        null
    }
}

class LSPathFinder2(start:IWorldRouter, maxVisited:Int, world:World) extends AStar(world)
{
    val pipe = start.getContainer
    val startBC = pipe.getCoords
    var visited = 0

    private var routers = Vector.newBuilder[StartEndPath]
    private var controller:ControllerPath = null

    def foundRouters = routers.result()
    def foundController = controller

    override def openInitials()
    {

        for (s <- 0 until 6) if (pipe.maskConnects(s)) open(new PathNode(pipe.getCoords, s))
    }

    override def evaluate(n:PathNode)
    {
        visited += 1
        if (visited > maxVisited) return
        val thisPipe = getPipe(n.bc)

        thisPipe match
        {
            case iwr:IWorldRouter =>
                val r = iwr.getRouter
                if (r != null && r.isLoaded)
                    routers += new StartEndPath(start.getRouter, r, n.hop, n.dist, thisPipe.routeFilter(n.dir^1))
            case p:FlowingPipePart =>
                for (s <- 0 until 6) if ((s^1) != n.dir && thisPipe.maskConnects(s))
                {
                    val pConn = getPipe(n.bc.copy().offset(s))
                    if (pConn != null) open(n --> (s, p.routeWeight))
                }
            case null => if (nextToStart(n)) //Tile interactions, power, etc.
            {
                val tile = getTile(n.bc)
                val link = LSPathFinder.getLinkState(tile)
                if (link != null) //Special LS
                {
                    val te = link.getLink(tile)
                    if (te != null)
                    {
                        val bc = new BlockCoord(te)
                        val linkedPipe = getPipe(bc)
                        if (linkedPipe != null) open(n --> (bc, linkedPipe.routeWeight)) //TODO ask linkstate for route weight
                    }
                }
                else tile match
                {
                    case layer:TControllerLayer if n.dir == 0 && controller == null => //Controller conn
                        controller = new ControllerPath(layer)
                    case _ =>
                }
            }
        }
    }

    override def isClosed(n:PathNode) = super.isClosed(n) || n.bc == startBC

    def getPipe(bc:BlockCoord) = PRLib.getMultiPart(world, bc, 6) match
    {
        case p:FlowingPipePart => p
        case _ => null
    }

    def getTile(bc:BlockCoord) = world.getTileEntity(bc.x, bc.y, bc.z)

    def nextToStart(n:PathNode) = startBC.copy.sub(n.bc).mag() == 1D
}

class CollectionPathFinder
{
    private var collectBroadcasts:Boolean = false
    private var collectCrafts:Boolean = false
    private var requester:IWorldRequester = null
    private var collected:Map[ItemKey, Int] = null

    def setCollectBroadcasts(flag:Boolean) =
    {
        collectBroadcasts = flag
        this
    }

    def setCollectCrafts(flag:Boolean) =
    {
        collectCrafts = flag
        this
    }

    def setRequester(requester:IWorldRequester) =
    {
        this.requester = requester
        this
    }

    def collect =
    {
        var pool = new ItemQueue
        val builder = new ItemQueue

        for (p <- requester.getRouter.getRoutesByCost)
        {
            builder.clear()
            p.end.getParent match
            {
                case c:IWorldCrafter if collectCrafts && p.flagRouteFrom && p.allowCrafting =>
                    c.getBroadcasts(builder)
                    val list = c.getCraftedItems
                    if (list != null) for (stack <- list) builder += stack.key -> 0

                case b:IWorldBroadcaster if collectBroadcasts && p.flagRouteFrom && p.allowBroadcast =>
                    b.getBroadcasts(builder)

                case _ =>
            }
            pool ++= builder.result.filter(i => p.allowItem(i._1))
        }
        collected = pool.result
        this
    }

    def getCollection = collected
}

object LogisticPathFinder
{
    def sharesInventory(pipe1:RoutedJunctionPipePart, pipe2:RoutedJunctionPipePart):Boolean =
    {
        if (pipe1 == null || pipe2 == null) return false
        if (pipe1.tile.getWorldObj != pipe2.tile.getWorldObj) return false

        val adjacent1 = pipe1.getInventory
        val adjacent2 = pipe2.getInventory
        if (adjacent1 == null || adjacent2 == null) return false

        adjacent1 == adjacent2
    }
}

class LogisticPathFinder(source:Router, payload:ItemKey)
{
    private var result:SyncResponse = null
    private var exclusions = BitSet()
    private var excludeSource = false

    private var visited = BitSet()

    def setExclusions(exc:BitSet) =
    {
        exclusions = exc
        this
    }

    def setExcludeSource(flag:Boolean) =
    {
        excludeSource = flag
        this
    }

    def getResult = result

    def findBestResult =
    {
        var bestResponse = new SyncResponse
        var bestIP = -1
        import mrtjp.projectred.core.lib.LabelBreaks._

        for (l <- source.getFilteredRoutesByCost(p => p.flagRouteTo && p.allowRouting && p.allowItem(payload))) label
        {
            val r = l.end
            if (excludeSource && r.getIPAddress == source.getIPAddress) break()
            if (excludeSource && LogisticPathFinder.sharesInventory(source.getParent.getContainer, r.getParent.getContainer)) break()
            if (exclusions(r.getIPAddress) || visited(r.getIPAddress)) break()

            visited += r.getIPAddress
            val parent = r.getParent
            if (parent == null) break()

            val sync = parent.getSyncResponse(payload, bestResponse)
            if (sync != null) if (sync.isPreferredOver(bestResponse))
            {
                bestResponse = sync
                bestIP = r.getIPAddress
            }
        }
        if (bestIP > -1) result = bestResponse.setResponder(bestIP)
        this
    }
}

class SyncResponse
{
    var priority = SendPriority.WANDERING
    var customPriority = 0
    var itemCount = 0
    var responder = -1

    def setPriority(p:SendPriority) =
    {
        priority = p
        this
    }

    def setCustomPriority(cp:Int) =
    {
        customPriority = cp
        this
    }

    def setItemCount(count:Int) =
    {
        itemCount = count
        this
    }

    def setResponder(r:Int) =
    {
        responder = r
        this
    }

    override def equals(other:Any) = other match
    {
        case that:SyncResponse =>
                priority == that.priority &&
                customPriority == that.customPriority &&
                itemCount == that.itemCount &&
                responder == that.responder
        case _ => false
    }

    override def hashCode() =
    {
        val state = Seq(priority, customPriority, itemCount, responder)
        state.map(_.hashCode()).foldLeft(0)((a, b) => 31*a+b)
    }

    def isPreferredOver(that:SyncResponse) = SyncResponse.isPreferredOver(priority.ordinal, customPriority, that)
}

object SyncResponse
{
    def isPreferredOver(priority:Int, customPriority:Int, that:SyncResponse) =
    {
        priority > that.priority.ordinal ||
            priority == that.priority.ordinal && customPriority > that.customPriority
    }
}
