package mrtjp.projectred.transportation
import java.util.UUID
import java.util.concurrent.PriorityBlockingQueue

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.vec.BlockCoord
import codechicken.multipart.TileMultipart
import mrtjp.core.inventory.InvWrapper
import mrtjp.core.item.{ItemKey, ItemKeyStack, ItemQueue}
import mrtjp.core.util.Pair2
import mrtjp.core.world.{Messenger, WorldLib}
import mrtjp.projectred.api.IConnectable
import mrtjp.projectred.core.Configurator
import mrtjp.projectred.transportation.Priorities.NetworkPriority
import mrtjp.projectred.transportation.TNetworkPipe.TransitComparator
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.IInventory
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.{IIcon, MovingObjectPosition}
import net.minecraft.world.World

import scala.collection.JavaConversions._
import scala.collection.immutable.BitSet


trait IWorldRouter
{
    def getRouter:Router

    def needsWork:Boolean

    def refreshState:Boolean

    def getContainer:TNetworkPipe
    def getWorld:World
    def getCoords:BlockCoord

    /** Item Syncing **/
    def itemEnroute(r:PipePayload)
    def itemArrived(r:PipePayload)
    def getSyncResponse(item:ItemKey, rival:SyncResponse):SyncResponse

    /** Item Requesting **/
    // Handled via subclasses.

    /** Item Broadcasting **/
    // Handled via subclasses.

    /** Item Crafting **/
    // Handled via subclasses.
}

trait IWorldRequester extends IWorldRouter
{
    def trackedItemLost(s:ItemKeyStack)

    def trackedItemReceived(s:ItemKeyStack)

    def getActiveFreeSpace(item:ItemKey):Int
}

trait IWorldBroadcaster extends IWorldRouter
{
    def requestPromises(request:RequestBranchNode, existingPromises:Int)

    def deliverPromises(promise:DeliveryPromise, requester:IWorldRequester)

    def operate(req:RequestMain){}//TODO
    def act(action:SupplyAction){}//TODO

    def getBroadcasts(col:ItemQueue){}

    def getBroadcastPriority:Int

    def getWorkLoad:Double
}

trait IWorldCrafter extends IWorldRequester with IWorldBroadcaster
{
    def buildCraftPromises(item:ItemKey):Vector[CraftingPromise]

    def registerExcess(promise:DeliveryPromise)

    def actOnExcess(action:SupplyAction){}//TODO

    def getCraftedItems:Vector[ItemKeyStack]

    def itemsToProcess:Int
}

trait TRouteLayer
{
    def queueStackToSend(stack:ItemStack, dirOfExtraction:Int, path:SyncResponse)
    {
        queueStackToSend(stack, dirOfExtraction, path.priority, path.responder)
    }
    def queueStackToSend(stack:ItemStack, dirOfExtraction:Int, priority:NetworkPriority, destination:Int)
    def getLogisticPath(stack:ItemKey, exclusions:BitSet, excludeStart:Boolean):SyncResponse

    def getRouter:Router
    def getWorldRouter:IWorldRouter
    def getBroadcaster:IWorldBroadcaster
    def getRequester:IWorldRequester

    def getWorld:World
    def getCoords:BlockCoord
}

trait TNetworkTravelConditions extends TPipeTravelConditions
{
    /**
     * 0CBR
     * R - allow Routing
     * B - allow Broadcastint
     * C - allow Crafting
     */
    def networkFilter = 0x7
}

trait TNetworkSubsystem extends PayloadPipePart
{
    override def canConnectPart(part:IConnectable, s:Int) = part match
    {
        case p:TNetworkSubsystem => true
        case _ => super.canConnectPart(part, s)
    }
}

trait TNetworkPipe extends PayloadPipePart with TInventoryPipe with IWorldRouter with TRouteLayer with IWorldRequester with TNetworkTravelConditions with TNetworkSubsystem
{
    var searchDelay =
    {
        TNetworkPipe.delayDelta += 1
        TNetworkPipe.delayDelta%Configurator.detectionFrequency
    }

    var linkMap:Byte = 0

    var router:Router = null
    var routerId:UUID = null

    val routerIDLock = new AnyRef

    var needsWork = true

    var sendQueue = Vector[PipePayload]()
    var transitQueue = new PriorityBlockingQueue[Pair2[PipePayload, Int]](10, TransitComparator)
    var swapQueue = Vector[PipePayload]()

    var statsReceived = 0
    var statsSent = 0
    var statsRelayed = 0

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setString("rid", getRouterId.toString)
        tag.setInteger("sent", statsSent)
        tag.setInteger("rec", statsReceived)
        tag.setInteger("relay", statsRelayed)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        routerIDLock synchronized
            {
                routerId = UUID.fromString(tag.getString("rid"))
            }

        statsSent = tag.getInteger("sent")
        statsReceived = tag.getInteger("rec")
        statsRelayed = tag.getInteger("relay")
    }

    override def writeDesc(packet:MCDataOutput)
    {
        super.writeDesc(packet)
        packet.writeByte(linkMap)
        packet.writeLong(getRouterId.getMostSignificantBits)
        packet.writeLong(getRouterId.getLeastSignificantBits)
    }

    override def readDesc(packet:MCDataInput)
    {
        super.readDesc(packet)
        linkMap = packet.readByte
        val mostSigBits = packet.readLong
        val leastSigBits = packet.readLong
        routerIDLock synchronized
            {
                routerId = new UUID(mostSigBits, leastSigBits)
            }
    }

    private def getRouterId =
    {
        if (routerId == null) routerIDLock synchronized
            {
                routerId = if (router != null) router.getID else UUID.randomUUID
            }
        routerId
    }

    private def removeFromTransitQueue(r:PipePayload)
    {
        transitQueue.removeAll(transitQueue.filter(p => p.get1 == r))
    }

    private def tickTransitQueue()
    {
        transitQueue.removeAll(transitQueue.filterNot(p =>
        {
            p.set2(p.get2-1)
            p.get2 >= 0
        }))
    }

    protected def countInTransit(key:ItemKey) =
    {
        transitQueue.filter(p => p.get1.payload.key == key).foldLeft(0)((b,a) => b+a.get2)
    }

    private def dispatchQueuedPayload(r:PipePayload)
    {
        injectPayload(r, r.input)
        val dest = RouterServices.getRouter(r.destinationIP)
        if (dest != null)
        {
            val wr = dest.getParent
            wr.itemEnroute(r)
            RouteFX.spawnType1(RouteFX.color_sync, 8, new BlockCoord(wr.getContainer.tile), world)
        }
        RouteFX.spawnType1(RouteFX.color_send, 8, new BlockCoord(tile), world)

        statsSent += 1
    }

    def queueSwapSendItem(r:PipePayload)
    {
        swapQueue :+= r
    }

    private def pollFromSwapQueue(stack:ItemKeyStack) =
    {
        swapQueue.find(_.payload == stack) match
        {
            case Some(r) =>
                val idx = swapQueue.indexOf(r)
                swapQueue = swapQueue.take(idx) ++ swapQueue.drop(idx+1)
                r
            case None => null
        }
    }

    final abstract override def update()
    {
        if (needsWork)
        {
            needsWork = false
            if (!world.isRemote) getRouter
            return
        }
        if (!world.isRemote) getRouter.update(world.getTotalWorldTime%Configurator.detectionFrequency == searchDelay)
        super.update()

        // Dispatch queued items
        while (sendQueue.nonEmpty) dispatchQueuedPayload(sendPoll())

        tickTransitQueue()

        if (world.isRemote) updateClient()
        else updateServer()

        def sendPoll() =
        {
            val first = sendQueue.head
            sendQueue = sendQueue.tail
            first
        }
    }

    protected def updateServer(){}

    protected def updateClient()
    {
        if (world.getTotalWorldTime%(Configurator.detectionFrequency*20) == searchDelay)
            for (i <- 0 until 6) if ((linkMap&1<<i) != 0)
                RouteFX.spawnType3(RouteFX.color_blink, 1, i, getCoords, world)
    }

    override def refreshState:Boolean =
    {
        if (world.isRemote) return false
        var link = 0
        for (s <- 0 until 6) if (getRouter.LSAConnectionExists(s)) link |= 1<<s
        if (linkMap != link)
        {
            linkMap = link.asInstanceOf[Byte]
            sendLinkMapUpdate()
            return true
        }
        false
    }

    override def getContainer = this

    override def endReached(r:PipePayload)
    {
        if (!world.isRemote) if (!maskConnects(r.output) || !passToNextPipe(r))
        {
            val bc = new BlockCoord(tile).offset(r.output)
            val t = WorldLib.getTileEntity(world, bc)
            val state = LSPathFinder.getLinkState(t)

            if (state != null && t.isInstanceOf[IInventory])
            {
                val dest = state.getLink(t)
                val inv = t.asInstanceOf[IInventory]
                if (dest.isInstanceOf[TileMultipart])
                {
                    val part = dest.asInstanceOf[TileMultipart].partMap(6)
                    if (part.isInstanceOf[TNetworkPipe])
                    {
                        val pipe = part.asInstanceOf[TNetworkPipe]
                        val w = InvWrapper.wrap(inv).setSlotsFromSide(r.output^1)
                        val room = w.getSpaceForItem(r.payload.key)
                        if (room >= r.payload.stackSize)
                        {
                            w.injectItem(r.payload.makeStack, true)
                            pipe.queueSwapSendItem(r)
                            return
                        }
                        else
                        {
                            bounceStack(r)
                            return
                        }
                    }
                }
            }
            val inv = InvWrapper.getInventory(world, bc)
            if (inv != null)
            {
                val w = InvWrapper.wrap(inv).setSlotsFromSide(r.output^1)
                r.payload.stackSize -= w.injectItem(r.payload.makeStack, true)
            }
            if (r.payload.stackSize > 0) bounceStack(r)
        }
    }

    override def read(packet:MCDataInput, key:Int) = key match
    {
        case 5 => handleLinkMap(packet)
        case _ => super.read(packet, key)
    }

    def sendLinkMapUpdate()
    {
        getWriteStreamOf(5).writeByte(linkMap)
    }

    private def handleLinkMap(packet:MCDataInput)
    {
        val old = linkMap
        linkMap = packet.readByte
        val high = ~old&linkMap
        val low = ~linkMap&old
        val bc = getCoords

        for (i <- 0 until 6)
        {
            if ((high&1<<i) != 0) RouteFX.spawnType3(RouteFX.color_linked, 1, i, bc, world)
            if ((low&1<<i) != 0) RouteFX.spawnType3(RouteFX.color_unlinked, 1, i, bc, world)
        }

        tile.markRender()
    }

    override def onRemoved()
    {
        super.onRemoved()
        TNetworkPipe.delayDelta = Math.max(TNetworkPipe.delayDelta-1, 0)
        val r = getRouter
        if (r != null) r.decommission()
    }

    override def activate(player:EntityPlayer, hit:MovingObjectPosition, item:ItemStack):Boolean =
    {
        if (super.activate(player, hit, item)) return true

        if (item != null && item.getItem.isInstanceOf[ItemRouterUtility])
        {
            if (!world.isRemote)
            {
                val s = "/#f"+"R"+getRouter.getIPAddress+" route statistics: "+
                        "\nreceived: "+statsReceived+
                        "\nsent: "+statsSent+
                        "\nrelayed: "+statsRelayed+
                        "\n\nroute table size: "+getRouter.getRouteTable.foldLeft(0)((b, v) => if (v != null) b+1 else b)
                val packet = Messenger.createPacket
                packet.writeDouble(x+0.0D)
                packet.writeDouble(y+0.5D)
                packet.writeDouble(z+0.0D)
                packet.writeString(s)
                packet.sendToPlayer(player)
            }
            return true
        }

        false
    }

    override def getIcon(side:Int):IIcon =
    {
        val array = PipeDefs.ROUTEDJUNCTION.sprites
        val ind = if (side == inOutSide) 2 else 0
        if ((linkMap&1<<side) != 0) array(1+ind)
        else array(2+ind)
    }

    override def resolveDestination(r:PipePayload)
    {
        if (needsWork) return
        var color = -1
        r.output = 6

        def reRoute(r:PipePayload)
        {
            r.resetTrip()
            val f = new LogisticPathFinder(getRouter, r.payload.key).setExclusions(r.travelLog).findBestResult
            if (f.getResult != null)
            {
                r.setDestination(f.getResult.responder, f.getResult.priority)
                color = RouteFX.color_route
            }
        }

        // Reroute if needed
        r.refreshIP()
        if (r.destinationIP <= 0 || (r.destinationIP > 0 && r.hasArrived)) reRoute(r)
        r.refreshIP()

        // Deliver item, or reroute
        if (r.destinationIP > 0 && r.destinationUUID == getRouter.getID)
        {
            r.output = getDirForIncomingItem(r)
            if (r.output == 6) reRoute(r)
            else
            {
                color = RouteFX.color_receive
                r.hasArrived = true
                itemArrived(r)
            }
            r.travelLog += getRouter.getIPAddress
        }

        // Relay item
        if (r.output == 6)
        {
            r.output = getRouter.getDirection(r.destinationIP, r.payload.key, r.netPriority)
            color = RouteFX.color_relay
        }

        // Set to wander, clear log for re-push
        if (r.output == 6)
        {
            super.resolveDestination(r)
            r.resetTrip()
            r.travelLog = BitSet()
            color = RouteFX.color_routeLost
        }

        if (color == RouteFX.color_relay) statsRelayed += 1
        RouteFX.spawnType1(color, 8, new BlockCoord(tile), world)
        adjustSpeed(r)
    }

    def getDirForIncomingItem(r:PipePayload):Int = inOutSide

    override def adjustSpeed(r:PipePayload)
    {
        r.speed = r.netPriority.boost
    }

    override def getRouter:Router =
    {
        if (needsWork) return null
        if (router == null) routerIDLock synchronized
            {
                router = RouterServices.getOrCreateRouter(getRouterId, this)
            }
        router
    }

    override def itemEnroute(r:PipePayload)
    {
        transitQueue.add(new Pair2(r, 200))
    }

    override def itemArrived(r:PipePayload)
    {
        removeFromTransitQueue(r)
        trackedItemReceived(r.payload)
        statsReceived+=1
    }

    override def getSyncResponse(item:ItemKey, rival:SyncResponse):SyncResponse = null

    override def queueStackToSend(stack:ItemStack, dirOfExtraction:Int, priority:NetworkPriority, destination:Int)
    {
        val stack2 = ItemKeyStack.get(stack)
        var r = pollFromSwapQueue(stack2)
        if (r == null)
        {
            r = PipePayload(stack2)
            r.input = dirOfExtraction
            r.setDestination(destination, priority)
        }
        sendQueue :+= r
    }

    override def getLogisticPath(stack:ItemKey, exclusions:BitSet, excludeStart:Boolean) =
    {
        val p = new LogisticPathFinder(getRouter, stack)
        if (exclusions != null) p.setExclusions(exclusions)
        p.setExcludeSource(excludeStart).findBestResult
        p.getResult
    }

    override def getWorldRouter = this
    override def getBroadcaster = this match
    {
        case b:IWorldBroadcaster => b
        case _ => null
    }
    override def getRequester = this

    override def getWorld = world
    override def getCoords = new BlockCoord(tile)

    override def trackedItemLost(s:ItemKeyStack){}
    override def trackedItemReceived(s:ItemKeyStack){}

    override def getActiveFreeSpace(item:ItemKey) =
    {
        val real = getInventory
        if (real == null) 0
        else InvWrapper.wrap(real).setSlotsFromSide(getInterfacedSide).getSpaceForItem(item)
    }
}

object TNetworkPipe
{
    var delayDelta = 0

    object TransitComparator extends Ordering[Pair2[PipePayload, Int]]
    {
        def compare(a:Pair2[PipePayload, Int], b:Pair2[PipePayload, Int]) =
        {
            var c = b.get2-a.get2
            if (c == 0) c = b.get1.payload.compareTo(a.get1.payload)
            c
        }
    }
}

class BasicPipePart extends BasicPipeAbstraction with TNetworkSubsystem
class RoutedJunctionPipePart extends BasicPipeAbstraction with TNetworkPipe