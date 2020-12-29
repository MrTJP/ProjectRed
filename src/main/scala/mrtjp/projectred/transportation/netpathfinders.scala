package mrtjp.projectred.transportation

import codechicken.multipart.BlockMultipart
import mrtjp.core.item.{ItemKey, ItemQueue}
import mrtjp.projectred.api.ISpecialLinkState
import mrtjp.projectred.core.Configurator
import mrtjp.projectred.transportation.Priorities.NetworkPriority
import net.minecraft.tileentity.TileEntity
import net.minecraft.util.EnumFacing
import net.minecraft.util.math.BlockPos

import scala.annotation.tailrec
import scala.collection.immutable.{BitSet, Queue}
import scala.collection.mutable.{Builder => MBuilder}

object LSPathFinder
{
    var start:IRouterContainer = _

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
        val pipe = start.getPipe
        val pos = pipe.pos
        val q = Queue.newBuilder[Node]
        for (s <- 0 until 6 if pipe.maskConnects(s)) q += Node(pos, s)
        iterate(q.result()).sorted
    }

    @tailrec
    private def iterate(open:Seq[Node], closed:Set[Node] = Set.empty,
                        coll:MBuilder[StartEndPath, Vector[StartEndPath]] =
                        Vector.newBuilder[StartEndPath]):Vector[StartEndPath] = open match
    {
        case _ if closed.size > Configurator.maxDetectionCount => coll.result()
        case Seq() => coll.result()
        case Seq(next, rest@_*) => getPipe(next.pos) match
        {
            case iwr:IRouterContainer with TNetworkPipe if {val r = iwr.getRouter; r != null && r.isLoaded} =>
                iterate(rest, closed+next, coll += new StartEndPath(start.getRouter,
                    iwr.getRouter, next.hop, next.dist, next.filters+iwr.pathFilter, iwr.networkFilter))
            case p:TNetworkSubsystem =>
                val upNext = Vector.newBuilder[Node]
                for (s <- 0 until 6) if (s != (next.dir^1) && p.maskConnects(s))
                {
                    val route = next --> (s, p.getPathWeight, p.pathFilter(next.dir^1, s))
                    if (route.path.pathFlags != 0 && !closed(route)) upNext += route
                }
                iterate(rest++upNext.result(), closed+next, coll)
            case _ =>
                val upNext = Vector.newBuilder[Node]
                val tile = getTile(next.pos)
                val link = LSPathFinder.getLinkState(tile)
                if (link != null) //Special LS
                {
                    val te = link.getLink(tile)
                    if (te != null)
                    {
                        val linkedPipe = getPipe(te.getPos)
                        if (linkedPipe != null)
                        {
                            val route = next --> (te.getPos, linkedPipe.getPathWeight)
                            if (!closed(route)) upNext += route
                        }
                    }
                }
                iterate(rest++upNext.result(), closed+next, coll) //Pipe connected to nothing?
        }
    }

    private def world = start.getWorld

    private def getPipe(pos:BlockPos) = BlockMultipart.getPart(world, pos, 6) match
    {
        case p:TNetworkSubsystem => p
        case _ => null
    }

    private def getTile(pos:BlockPos) = world.getTileEntity(pos)

    private object Node
    {
        def apply(pos:BlockPos, dir:Int):Node = Node(pos.offset(EnumFacing.values()(dir)), 1, dir, dir)
    }
    private case class Node(pos:BlockPos, dist:Int, dir:Int, hop:Int, filters:Set[PathFilter] = Set.empty) extends Ordered[Node]
    {
        val path = new Path(filters)

        def -->(to:Node, toDir:Int):Node = Node(to.pos, dist+to.dist, toDir, hop)

        def -->(toDir:Int, distAway:Int, filter:PathFilter):Node = Node(pos.offset(EnumFacing.values()(toDir)), dist+distAway, toDir, hop, filters+filter)
        def -->(toDir:Int, distAway:Int):Node = Node(pos.offset(EnumFacing.values()(toDir)), dist+distAway, toDir, hop, filters)
        def -->(toDir:Int):Node = this -->(toDir, 1)

        def -->(to:BlockPos, distAway:Int, filter:PathFilter):Node = Node(to, dist+distAway, dir, hop, filters+filter)
        def -->(to:BlockPos, distAway:Int):Node = Node(to, dist+distAway, dir, hop, filters)
        def -->(to:BlockPos):Node = this -->(to, 1)

        override def compare(that:Node) = dist-that.dist

        override def equals(other:Any) = other match
        {
            case that:Node => pos == that.pos && hop == that.hop
            case _ => false
        }

        override def hashCode = pos.hashCode*7+hop
    }
}

object CollectionPathFinder
{
    var start:IRouterContainer = null
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
            val parent = p.end.getContainer

            if (collectCrafts && p.flagRouteFrom && p.allowCrafting)
            {
                val list = parent.getCraftedItems
                if (list != null) for (stack <- list) builder += stack.key -> 0
            }

            if (collectBroadcasts && p.flagRouteFrom && p.allowBroadcast)
                parent.getBroadcasts(builder)

            pool ++= builder.result.filter(i => p.allowItem(i._1))
        }
        pool.result
    }
}

object LogisticPathFinder
{
    var start:Router = _
    var payload:ItemKey = _

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

        for (l <- start.getFilteredRoutesByCost(p => p.flagRouteTo && p.allowRouting && p.allowItem(payload))) breakable {
            val r = l.end
            if (excludeSource && r.getIPAddress == start.getIPAddress) break()
            if (excludeSource && LogisticPathFinder.sharesInventory(start.getContainer.getPipe, r.getContainer.getPipe)) break()
            if (exclusions(r.getIPAddress) || visited(r.getIPAddress)) break()

            visited += r.getIPAddress
            val parent = r.getContainer
            if (parent == null) break()

            val sync = parent.getSyncResponse(payload, bestResponse)
            if (sync != null) if (sync.isPreferredOver(bestResponse)) {
                bestResponse = sync
                bestIP = r.getIPAddress
            }
        }
        if (bestIP > -1) bestResponse.setResponder(bestIP) else null
    }

    //TODO, Not sure if this will work with capabilities, some mods just return a new cap class every time..
    def sharesInventory(pipe1:TInventoryPipe[_], pipe2:TInventoryPipe[_]):Boolean =
    {
        if (pipe1 == null || pipe2 == null) return false
        if (pipe1.tile.getWorld != pipe2.tile.getWorld) return false

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
