package mrtjp.projectred.transportation

import codechicken.lib.vec.BlockCoord
import codechicken.multipart.TileMultipart
import java.util
import mrtjp.projectred.api.ISpecialLinkState
import mrtjp.projectred.core.utils.{ItemKey, Pair2}
import net.minecraft.tileentity.TileEntity
import net.minecraftforge.common.ForgeDirection
import scala.collection
import scala.collection.immutable.{BitSet, HashMap, HashSet}
import scala.collection.immutable.BitSet.BitSet1

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
    private var setVisited = HashSet[BasicPipePart]()

    val LSAddresser = start.getRouter
    var result = getConnectedRoutingPipes(start.getContainer, side)


    private def getConnectedRoutingPipes(start:BasicPipePart, side:ForgeDirection):HashMap[Router, StartEndPath] =
    {
        var foundPipes = HashMap[Router, StartEndPath]()
        val root = setVisited.isEmpty

        if (setVisited.size == 1) pipesVisited = 0

        pipesVisited += 1
        if (pipesVisited > maxVisited) return foundPipes
        if (setVisited.size > maxLength) return foundPipes
        if (!start.initialized) return foundPipes

        if (start.isInstanceOf[IWorldRouter] && !root)
        {
            val r = (start.asInstanceOf[IWorldRouter]).getRouter
            if (!r.isLoaded) return foundPipes
            foundPipes += (r -> new StartEndPath(LSAddresser, r, side.getOpposite.ordinal, setVisited.size))
            return foundPipes
        }
        setVisited += start

        val connections = new java.util.ArrayDeque[Pair2[TileEntity, ForgeDirection]]

        import mrtjp.projectred.core.utils.LabelBreaks._
        for (dir <- ForgeDirection.VALID_DIRECTIONS) label("1")
        {
            if (root && side != ForgeDirection.UNKNOWN && !(dir == side)) break("1")
            if (!start.maskConnects(dir.ordinal)) break("1")
            val bc = new BlockCoord(start.tile).offset(dir.ordinal)
            val tile = start.world.getBlockTileEntity(bc.x, bc.y, bc.z)
            connections.add(new Pair2[TileEntity, ForgeDirection](tile, dir))
        }

        while (!connections.isEmpty) label("1")
        {
            val pair:Pair2[TileEntity, ForgeDirection] = connections.pollFirst
            val tile:TileEntity = pair.getValue1
            val dir:ForgeDirection = pair.getValue2
            if (root)
            {
                val link:ISpecialLinkState = LSPathFinder.getLinkState(tile)
                val connected = if (link == null) null else link.getLink(tile)
                if (connected != null)
                {
                    connections.add(new Pair2[TileEntity, ForgeDirection](connected, dir))
                    break("1")
                }
            }

            if (!(tile.isInstanceOf[TileMultipart])) break("1")
            val tile2 = tile.asInstanceOf[TileMultipart]
            val part = tile2.partMap(6)
            var p:BasicPipePart = null
            if (part.isInstanceOf[BasicPipePart]) p = part.asInstanceOf[BasicPipePart]

            if (p == null) break("1")
            if (setVisited.contains(p)) break("1")

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
        if (start.isInstanceOf[IWorldRouter])
            for (e <- foundPipes.values) e.start = start.asInstanceOf[IWorldRouter].getRouter

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
        val pool = collection.mutable.HashMap[ItemKey, Int]()

        for (l <- requester.getRouter.getRoutesByCost)
        {
            val r = l.end
            val wr = r.getParent
            if (wr.isInstanceOf[IWorldCrafter] && collectCrafts)
            {
                val c = wr.asInstanceOf[IWorldCrafter]
                c.getBroadcastedItems(pool)
                val list = c.getCraftedItems

                if (list != null)
                {
                    for (stack <- list) if (!pool.contains(stack.key)) pool.put(stack.key, 0)
                }
            }
            else if (wr.isInstanceOf[IWorldBroadcaster] && collectBroadcasts)
            {
                val b = wr.asInstanceOf[IWorldBroadcaster]
                b.getBroadcastedItems(pool)
            }
        }
        collected = HashMap() ++ pool
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

        for (l <- source.getRoutesByCost) label("1")
        {
            val r = l.end
            if (excludeSource && r.getIPAddress == source.getIPAddress) break("1")
            if (excludeSource && LogisticPathFinder.sharesInventory(source.getParent.getContainer, r.getParent.getContainer)) break("1")
            if (exclusions(r.getIPAddress) || visited(r.getIPAddress)) break("1")

            visited += r.getIPAddress
            val parent = r.getParent
            if (parent == null) break("1")

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
