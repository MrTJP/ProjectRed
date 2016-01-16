package mrtjp.projectred.transportation

import codechicken.lib.vec.BlockCoord
import mrtjp.core.item.{ItemQueue, ItemKey}
import mrtjp.projectred.api.ISpecialLinkState
import mrtjp.projectred.core.Configurator
import mrtjp.projectred.core.libmc.PRLib
import mrtjp.projectred.transportation.Priorities.NetworkPriority
import net.minecraft.tileentity.TileEntity

import scala.annotation.tailrec
import scala.collection.immutable.{BitSet, Queue}
import scala.collection.mutable.{Builder => MBuilder}

object LSPathFinder
{
    var start:IWorldRouter = null

    private var registeredLSTypes = List[ISpecialLinkState]()

    def register(link:ISpecialLinkState)
    {
        registeredLSTypes :+= link
    }

    def getLinkState(tile:TileEntity):ISpecialLinkState =
    {
        if (tile == null) return null
        for (l <- registeredLSTypes) if (l.matches(tile)) return l
        null
    }

    def clear()
    {
        start = null
    }

    def result() =
    {
        val pipe = start.getContainer
        val bc = start.getCoords
        val q = Queue.newBuilder[Node]
        for (s <- 0 until 6 if pipe.maskConnects(s)) q += Node(bc, s)
        iterate(q.result())
    }

    @tailrec
    private def iterate(open:Seq[Node], closed:Set[Node] = Set.empty,
                        coll:MBuilder[StartEndPath, Vector[StartEndPath]] =
                        Vector.newBuilder[StartEndPath]):Vector[StartEndPath] = open match
    {
        case _ if closed.size > Configurator.maxDetectionCount => coll.result()
        case Seq() => coll.result()
        case Seq(next, rest@_*) => getPipe(next.bc) match
        {
            case iwr:IWorldRouter with TNetworkPipe if {val r = iwr.getRouter; r != null && r.isLoaded} =>
                iterate(rest, closed+next, coll += new StartEndPath(start.getRouter,
                    iwr.getRouter, next.hop, next.dist, next.filters+iwr.pathFilter, iwr.networkFilter))
            case p:TNetworkSubsystem =>
                val upNext = Vector.newBuilder[Node]
                for (s <- 0 until 6) if (s != (next.dir^1) && p.maskConnects(s))
                {
                    val route = next --> (s, p.pathWeight, p.pathFilter(next.dir^1, s))
                    if (route.path.pathFlags != 0 && !closed(route)) upNext += route
                }
                iterate(rest++upNext.result(), closed+next, coll)
            case _ =>
                val upNext = Vector.newBuilder[Node]
                val tile = getTile(next.bc)
                val link = LSPathFinder.getLinkState(tile)
                if (link != null) //Special LS
                {
                    val te = link.getLink(tile)
                    if (te != null)
                    {
                        val bc = new BlockCoord(te)
                        val linkedPipe = getPipe(bc)
                        if (linkedPipe != null)
                        {
                            val route = next -->(bc, linkedPipe.pathWeight)
                            if (!closed(route)) upNext += route
                        }
                    }
                }
                iterate(rest++upNext.result(), closed+next, coll) //Pipe connected to nothing?
        }
    }

    private def world = start.getWorld

    private def getPipe(bc:BlockCoord) = PRLib.getMultiPart(world, bc, 6) match
    {
        case p:TNetworkSubsystem => p
        case _ => null
    }

    private def getTile(bc:BlockCoord) = world.getTileEntity(bc.x, bc.y, bc.z)

    private object Node
    {
        def apply(bc:BlockCoord, dir:Int):Node = Node(bc.copy().offset(dir), 1, dir, dir)
    }
    private case class Node(bc:BlockCoord, dist:Int, dir:Int, hop:Int, filters:Set[PathFilter] = Set.empty) extends Ordered[Node]
    {
        val path = new Path(filters)

        def -->(to:Node, toDir:Int):Node = Node(to.bc, dist+to.dist, toDir, hop)

        def -->(toDir:Int, distAway:Int, filter:PathFilter):Node = Node(bc.copy.offset(toDir), dist+distAway, toDir, hop, filters+filter)
        def -->(toDir:Int, distAway:Int):Node = Node(bc.copy().offset(toDir), dist+distAway, toDir, hop, filters)
        def -->(toDir:Int):Node = this -->(toDir, 1)

        def -->(to:BlockCoord, distAway:Int, filter:PathFilter):Node = Node(to, dist+distAway, dir, hop, filters+filter)
        def -->(to:BlockCoord, distAway:Int):Node = Node(to, dist+distAway, dir, hop, filters)
        def -->(to:BlockCoord):Node = this -->(to, 1)

        override def compare(that:Node) = dist-that.dist

        override def equals(other:Any) = other match
        {
            case that:Node => bc == that.bc && hop == that.hop
            case _ => false
        }

        override def hashCode = bc.hashCode*7+hop
    }
}

object CollectionPathFinder
{
    var start:IWorldRequester = null
    var collectBroadcasts:Boolean = false
    var collectCrafts:Boolean = false

    def clear()
    {
        start = null
        collectBroadcasts = false
        collectCrafts = false
    }

    def result():Map[ItemKey, Int] =
    {
        var pool = new ItemQueue
        val builder = new ItemQueue

        for (p <- start.getRouter.getRoutesByCost)
        {
            builder.clear()
            val parent = p.end.getParent

            if (parent.isInstanceOf[IWorldCrafter] && collectCrafts && p.flagRouteFrom && p.allowCrafting)
            {
                val list = parent.asInstanceOf[IWorldCrafter].getCraftedItems
                if (list != null) for (stack <- list) builder += stack.key -> 0
            }

            if (parent.isInstanceOf[IWorldBroadcaster] && collectBroadcasts && p.flagRouteFrom && p.allowBroadcast)
                parent.asInstanceOf[IWorldBroadcaster].getBroadcasts(builder)

            pool ++= builder.result.filter(i => p.allowItem(i._1))
        }
        pool.result
    }
}

object LogisticPathFinder
{
    var start:Router = null
    var payload:ItemKey = null

    var exclusions = BitSet.empty
    var excludeSource = false

    private var visited = BitSet.empty

    def clear()
    {
        start = null
        payload = null
        exclusions = BitSet.empty
        excludeSource = false
        visited = BitSet.empty
    }

    def result():SyncResponse =
    {
        var bestResponse = new SyncResponse
        var bestIP = -1
        import scala.util.control.Breaks._

        for (l <- start.getFilteredRoutesByCost(p => p.flagRouteTo && p.allowRouting && p.allowItem(payload))) breakable
        {
            val r = l.end
            if (excludeSource && r.getIPAddress == start.getIPAddress) break()
            if (excludeSource && LogisticPathFinder.sharesInventory(start.getParent.getContainer, r.getParent.getContainer)) break()
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
        if (bestIP > -1) bestResponse.setResponder(bestIP) else null
    }

    def sharesInventory(pipe1:TInventoryPipe[_], pipe2:TInventoryPipe[_]):Boolean =
    {
        if (pipe1 == null || pipe2 == null) return false
        if (pipe1.tile.getWorldObj != pipe2.tile.getWorldObj) return false

        val adjacent1 = pipe1.getInventory
        val adjacent2 = pipe2.getInventory
        if (adjacent1 == null || adjacent2 == null) return false

        adjacent1 == adjacent2
    }
}

class SyncResponse
{
    var priority = Priorities.WANDERING
    var customPriority = 0
    var itemCount = Int.MaxValue
    var responder = -1

    def setPriority(p:NetworkPriority) =
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
