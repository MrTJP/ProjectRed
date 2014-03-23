package mrtjp.projectred.transportation

import codechicken.lib.vec.BlockCoord
import codechicken.multipart.TileMultipart
import mrtjp.projectred.api.ISpecialLinkState
import mrtjp.projectred.core.utils.{ItemKey, Pair2}
import mrtjp.projectred.transportation.SendPriority.SendPriority
import net.minecraft.tileentity.TileEntity
import net.minecraftforge.common.ForgeDirection
import scala.collection.mutable.{HashMap => MHashMap}
import scala.collection.immutable.{BitSet, HashMap, HashSet}

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

class LSPathFinder(start:IWorldRouter, maxVisited:Int, maxLength:Int, side:ForgeDirection)
{
    def this(start:IWorldRouter, maxVisited:Int, maxLength:Int) =
        this(start, maxVisited, maxLength, ForgeDirection.UNKNOWN)

    private var pipesVisited = 0
    private var setVisited = HashSet[FlowingPipePart]()

    val LSAddresser = start.getContainer
    var result = getConnectedRoutingPipes(start.getContainer, side)

    private def getConnectedRoutingPipes(start:FlowingPipePart, side:ForgeDirection):HashMap[Router, StartEndPath] =
    {
        var foundPipes = HashMap[Router, StartEndPath]()
        val root = setVisited.isEmpty

        if (setVisited.size == 1) pipesVisited = 0
        pipesVisited += 1

        if (pipesVisited > maxVisited) return foundPipes
        if (setVisited.size > maxLength) return foundPipes
        if (!start.initialized) return foundPipes

        start match
        {
            case wr:IWorldRouter if !root =>
                val r = wr.getRouter
                if (!r.isLoaded) return foundPipes
                foundPipes += (r -> new StartEndPath(LSAddresser.getRouter, r, side.getOpposite.ordinal,
                    setVisited.size, start.routeFilter(side.ordinal()^1)))
                return foundPipes
            case _ =>
        }
        setVisited += start

        val connections = new java.util.ArrayDeque[Pair2[TileEntity, ForgeDirection]]

        import mrtjp.projectred.core.utils.LabelBreaks._
        for (dir <- ForgeDirection.VALID_DIRECTIONS) label
        {
            if (root && side != ForgeDirection.UNKNOWN && !(dir == side)) break()
            if (!start.maskConnects(dir.ordinal)) break()
            if (start.tile == null) return foundPipes //pipe broken during search

            val bc = new BlockCoord(start.tile).offset(dir.ordinal)
            val tile = start.world.getBlockTileEntity(bc.x, bc.y, bc.z)
            connections.add(new Pair2[TileEntity, ForgeDirection](tile, dir))
        }

        while (!connections.isEmpty) label
        {
            val pair = connections.pollFirst
            val tile = pair.getValue1
            val dir = pair.getValue2
            if (root)
            {
                val link = LSPathFinder.getLinkState(tile)
                val connected = if (link == null) null else link.getLink(tile)
                if (connected != null)
                {
                    connections.add(new Pair2[TileEntity, ForgeDirection](connected, dir))
                    break()
                }
            }

            if (!(tile.isInstanceOf[TileMultipart])) break()
            val tile2 = tile.asInstanceOf[TileMultipart]
            val part = tile2.partMap(6)
            val p = part match
            {
                case pipe:FlowingPipePart => pipe
                case _ => null
            }

            if (p == null) break()
            if (setVisited.contains(p)) break()

            val result = getConnectedRoutingPipes(p, dir)

            for ((k, v) <- result)
            {
                v.dirToFirstHop = dir.ordinal
                val found = foundPipes.getOrElse(k, null)
                val current = v.distance
                val previous = if (found == null) Integer.MAX_VALUE else found.distance
                if (current < previous) foundPipes += (k -> v)
            }
        }

        setVisited -= start
        //for (e <- foundPipes.values) e.start = LSAddresser.getRouter
        foundPipes
    }

    def getResult = result
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
        var pool = MHashMap[ItemKey, Int]()
        for (p <- requester.getRouter.getRoutesByCost)
        {
            p.end.getParent match
            {
                case c:IWorldCrafter if collectCrafts && p.allowCrafting =>
                    c.getBroadcastedItems(pool)
                    val list = c.getCraftedItems
                    if (list != null) for (stack <- list)
                        if (!pool.contains(stack.key)) pool += stack.key -> 0
                case b:IWorldBroadcaster if collectBroadcasts && p.allowBroadcast =>
                    b.getBroadcastedItems(pool)
                case _ =>
            }
            pool = pool.filter(i => p.allowItem(i._1))
        }
        collected = pool.toMap
        this
    }

    def getCollection = collected
}

object LogisticPathFinder
{
    def sharesInventory(pipe1:RoutedJunctionPipePart, pipe2:RoutedJunctionPipePart):Boolean =
    {
        if (pipe1 == null || pipe2 == null) return false
        if (pipe1.tile.worldObj ne pipe2.tile.worldObj) return false

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
        this.excludeSource = flag
        this
    }

    def getResult = result

    def findBestResult =
    {
        var bestResponse = new SyncResponse
        var bestIP = -1
        import mrtjp.projectred.core.utils.LabelBreaks._

        for (l <- source.getFilteredRoutesByCost(p => p.allowRouting && p.allowItem(payload))) label
        {
            val r = l.end
            if (excludeSource && r.getIPAddress == source.getIPAddress) break()
            if (excludeSource && LogisticPathFinder.sharesInventory(source.getParent.getContainer, r.getParent.getContainer)) break()
            if (exclusions(r.getIPAddress) || visited(r.getIPAddress)) break()

            visited += r.getIPAddress
            val parent = r.getParent
            if (parent == null) break()

            val sync = parent.getSyncResponse(payload, bestResponse)
            if (sync != null) if (sync.priority.ordinal > bestResponse.priority.ordinal)
            {
                bestResponse = sync
                bestIP = r.getIPAddress
            }
            else if (sync.priority.ordinal == bestResponse.priority.ordinal && sync.customPriority > bestResponse.customPriority)
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
}
