package mrtjp.projectred.transportation
import java.util.UUID

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.raytracer.CuboidRayTraceResult
import mrtjp.core.item.{ItemKey, ItemKeyStack, ItemQueue}
import mrtjp.core.world.Messenger
import mrtjp.projectred.api.IConnectable
import mrtjp.projectred.core.Configurator
import mrtjp.projectred.transportation.Priorities.NetworkPriority
import net.minecraft.client.renderer.texture.TextureAtlasSprite
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.EnumHand
import net.minecraft.world.World

import scala.collection.immutable.BitSet

trait IRouterContainer
{
    def getWorld:World
    def getPipe:TNetworkPipe
    def getRouter:Router

    def refreshState(sideMask:Int)

    def postNetworkEvent(e:NetworkEvent)

    /** Separation of function **/
    def searchForLinks:Vector[StartEndPath]

    /** TODO move all of the following to their own services **/

    /** Item Requesting **/
    def getActiveFreeSpace(item:ItemKey):Int

    /** Item Broadcasting **/
    def requestPromise(request:RequestBranchNode, existingPromises:Int){}
    def deliverPromise(promise:DeliveryPromise, requester:IRouterContainer){}
    def getBroadcasts(col:ItemQueue){}
    def getBroadcastPriority:Int = 0
    def getWorkLoad:Double = 0

    /** Item Crafting **/
    def requestCraftPromise(request:RequestBranchNode):Seq[CraftingPromise] = Seq()
    def registerExcess(promise:DeliveryPromise){}
    def getCraftedItems:Seq[ItemKeyStack] = Seq()
    def itemsToProcess:Int = 0

    /** Transport Layer **/
    def queueStackToSend(item:ItemKey, amount:Int, path:SyncResponse)
    {
        queueStackToSend(item, amount, path.priority, path.responder)
    }
    def queueStackToSend(item:ItemKey, amount:Int, priority:NetworkPriority, destination:Int)

    /** Session Layer **/
    def getLogisticPath(item:ItemKey, exclusions:BitSet, excludeStart:Boolean):SyncResponse
    def getSyncResponse(item:ItemKey, rival:SyncResponse):SyncResponse
}

trait PayloadResolution

case class RecievePayload() extends PayloadResolution
case class RoutePayload(toDir:Int) extends PayloadResolution
case class RelayPayload(toDir:Int) extends PayloadResolution
case class UnresolvedPayload() extends PayloadResolution

abstract class NetworkEvent(isCancelable:Boolean)
{
    private var canceled = false

    def isCanceled = canceled

    def setCanceled()
    {
        if (!isCancelable) throw new Exception(s"Network event ${this.getClass.getSimpleName} cannot be canceled")
        if (canceled) throw new Exception(s"Network event ${this.getClass.getSimpleName} is already canceled")
        canceled = true
    }
}

case class PayloadDepartedEvent(item:ItemKey, amount:Int, from:IRouterContainer) extends NetworkEvent(true)
{
    var remaining = amount
}

case class PayloadArrivedEvent(item:ItemKey, amount:Int) extends NetworkEvent(true)
{
    var remaining = amount
}

case class PayloadLostEnrouteEvent(item:ItemKey, amount:Int) extends NetworkEvent(true)
{
    var remaining = amount
}

case class TrackedPayloadPendingEvent(item:ItemKey, amount:Int, from:IRouterContainer) extends NetworkEvent(true)
{
    var remaining = amount
}

case class TrackedPayloadCancelledEvent(item:ItemKey, amount:Int, from:IRouterContainer) extends NetworkEvent(true)
{
    var remaining = amount
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

trait TNetworkSubsystem extends PayloadPipePart[NetworkPayload]
{
    override def canConnectPart(part:IConnectable, s:Int) = part match
    {
        case p:TNetworkSubsystem => true
        case _ => super.canConnectPart(part, s)
    }

    override def createNewPayload(id:Int) = new NetworkPayload(id)
}

trait TNetworkPipe extends PayloadPipePart[NetworkPayload] with TInventoryPipe[NetworkPayload] with IRouterContainer with TNetworkTravelConditions with TNetworkSubsystem
{
    val searchDelay = {
        TNetworkPipe.delayDelta += 1
        TNetworkPipe.delayDelta%Configurator.detectionFrequency
    }

    var linkMap:Byte = 0

    var router:Router = null
    var routerId:UUID = null

    val routerIDLock = new AnyRef

    var sendQueue = Seq[NetworkPayload]()
    var transitQueue = new ItemQueue

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
        routerIDLock synchronized{
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
        routerIDLock synchronized {
            routerId = new UUID(mostSigBits, leastSigBits)
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

        for (i <- 0 until 6)
        {
            if ((high&1<<i) != 0) RouteFX2.spawnType3(RouteFX2.color_linked, i, this)
            if ((low&1<<i) != 0) RouteFX2.spawnType2(RouteFX2.color_unlinked, i, this)
        }

        tile.markRender()
    }

    private def getRouterId =
    {
        if (routerId == null) routerIDLock synchronized {
            routerId = if (router != null) router.getID else UUID.randomUUID
        }
        routerId
    }

    override def getRouter:Router =
    {
        if (router == null) routerIDLock synchronized {
            router = RouterServices.getOrCreateRouter(getRouterId, this)
        }
        router
    }

    protected def countInTransit(key:ItemKey) = transitQueue(key)

    private def dispatchQueuedPayload(r:NetworkPayload)
    {
        injectPayload(r, r.input)
        val dest = RouterServices.getRouter(r.destinationIP)
        if (dest != null)
        {
            val wr = dest.getContainer
            wr.postNetworkEvent(PayloadDepartedEvent(r.payload.key, r.payload.stackSize, this))
            RouteFX2.spawnType1(RouteFX2.color_sync, this)
        }
        RouteFX2.spawnType1(RouteFX2.color_send, this)

        statsSent += 1
    }

    final abstract override def update()
    {
        super.update()

        if (!world.isRemote)
            getRouter.update(world.getTotalWorldTime)

        // Dispatch next item in queue
        if (sendQueue.nonEmpty) {
            val out = sendQueue.head
            sendQueue = sendQueue.tail
            dispatchQueuedPayload(out)
        }

        if (world.isRemote) updateClient()
        else updateServer()
    }

    protected def updateServer(){}

    protected def updateClient()
    {
        if (world.getTotalWorldTime%(Configurator.detectionFrequency*10) == searchDelay)
            for (i <- 0 until 6) if ((linkMap&1<<i) != 0)
                RouteFX2.spawnType3(RouteFX2.color_blink, i, this)
    }

    override def refreshState(sideMask:Int)
    {
        if (world.isRemote) return
        if (linkMap != sideMask) {
            linkMap = sideMask.toByte
            sendLinkMapUpdate()
        }
    }

    override def searchForLinks:Vector[StartEndPath] =
    {
        LSPathFinder.clear()
        LSPathFinder.start = this
        val newAdjacent = LSPathFinder.result()
        LSPathFinder.clear()
        newAdjacent
    }

    override def getPipe = this

    override def onRemoved()
    {
        super.onRemoved()
        TNetworkPipe.delayDelta = math.max(TNetworkPipe.delayDelta-1, 0)
        val r = getRouter
        if (r != null) r.decommision()
    }

    override def activate(player:EntityPlayer, hit:CuboidRayTraceResult, item:ItemStack, hand:EnumHand):Boolean =
    {
        if (super.activate(player, hit, item, hand)) return true

        if (!item.isEmpty && item.getItem.isInstanceOf[ItemRouterUtility]) {
            if (!world.isRemote) {
                val s = "/#f"+"R"+getRouter.getIPAddress+" route statistics: "+
                        "\nreceived: "+statsReceived+
                        "\nsent: "+statsSent+
                        "\nrelayed: "+statsRelayed+
                        "\n\nroute table size: "+getRouter.getRouteTable.foldLeft(0)((b, v) => if (v != null) b+1 else b)
                val packet = Messenger.createPacket
                packet.writeDouble(pos.getX+0.0D)
                packet.writeDouble(pos.getY+0.5D)
                packet.writeDouble(pos.getZ+0.0D)
                packet.writeString(s)
                packet.sendToPlayer(player)
            }
            return true
        }

        false
    }

    override def getIcon(side:Int):TextureAtlasSprite =
    {
        val array = PipeDefs.ROUTEDJUNCTION.sprites
        val ind = if (side == inOutSide) 2 else 0
        if ((linkMap&1<<side) != 0) array(1+ind)
        else array(2+ind)
    }

    override def resolveDestination(r:NetworkPayload)
    {
        var colour = getRouter.resolvePayload(r) match {
            case RoutePayload(toDir) =>
                r.output = toDir
                RouteFX2.color_route
            case RelayPayload(toDir) =>
                r.output = toDir
                RouteFX2.color_relay
            case RecievePayload() =>
                r.output = getDirForIncomingItem(r)
                if (r.output != 6)
                    postNetworkEvent(PayloadArrivedEvent(r.payload.key, r.payload.stackSize)) //TODO router should handle events now
                RouteFX2.color_receive
            case UnresolvedPayload() =>
                r.output = 6
                RouteFX2.color_routeLost
        }

        if (r.output == 6) {
            r.resetTrip()
            var m = 0
            for (i <- 0 until 6) if (getStraight(i).isInstanceOf[TNetworkSubsystem]) m |= 1<<i
            chooseRandomDestination(r, ~m)
            colour = RouteFX2.color_routeLost
        }

        adjustSpeed(r)
        RouteFX2.spawnType1(colour, this)

//        var color = -1
//
//        def reRoute() {
//            r.resetTrip()
//
//            LogisticPathFinder.clear()
//            LogisticPathFinder.start = getRouter
//            LogisticPathFinder.payload = r.payload.key
//            LogisticPathFinder.exclusions = r.travelLog
//            val result = LogisticPathFinder.result()
//            LogisticPathFinder.clear()
//
//            if (result != null)
//            {
//                r.setDestination(result.responder, result.priority)
//                color = RouteFX2.color_route
//            }
//        }
//
//        // Reroute if needed
//        r.refreshIP()
//        if (r.destinationIP <= 0 || r.hasArrived) reRoute()
//
//        // Deliver item, or reroute
//        if (r.destinationIP > 0 && r.destinationUUID == getRouter.getID) {
//            r.output = getDirForIncomingItem(r)
//            r.travelLog += getRouter.getIPAddress
//
//            if (r.output == 6) {
//                reRoute()
//            } else {
//                color = RouteFX2.color_receive
//                r.hasArrived = true
//                postNetworkEvent(PayloadArrivedEvent(r.payload.key, r.payload.stackSize))
//            }
//        }
//
//        if (r.destinationUUID != getRouter.getID) {
//            r.output = getRouter.getDirection(r.destinationIP, r.payload.key, r.netPriority)
//            color = RouteFX2.color_relay
//            if (r.output == 6) {
//                var m = 0
//                for (i <- 0 until 6) if (getStraight(i).isInstanceOf[TNetworkSubsystem]) m |= 1<<i
//                chooseRandomDestination(r, ~m)
//                r.resetTrip()
//                r.travelLog = BitSet.empty
//                color = RouteFX2.color_routeLost
//            }
//        }
//
//        adjustSpeed(r)
//
//        if (color == RouteFX2.color_relay) statsRelayed += 1
//        RouteFX2.spawnType1(color, this)
    }

    override def injectPayload(r:NetworkPayload, in:Int) =
    {
        super.injectPayload(r, in)
        if (r.netPriority == Priorities.WANDERING)
            r.tickPayloadWander()
    }

    def getDirForIncomingItem(r:NetworkPayload):Int = inOutSide

    override def adjustSpeed(r:NetworkPayload)
    {
        r.speed = r.netPriority.boost
    }

    override def postNetworkEvent(event:NetworkEvent)
    {
        event match {
            case e:PayloadDepartedEvent => transitQueue.add(e.item, e.amount)
            case e:PayloadArrivedEvent => transitQueue.remove(e.item, e.amount)
            case e:PayloadLostEnrouteEvent => transitQueue.remove(e.item, e.amount)
            case _ =>
        }
    }

    override def getSyncResponse(item:ItemKey, rival:SyncResponse):SyncResponse = null

    override def queueStackToSend(item:ItemKey, amount:Int, priority:NetworkPriority, destination:Int)
    {
        val stack2 = ItemKeyStack.get(item, amount)
        var r = new NetworkPayload(AbstractPipePayload.claimID())
        r.payload = stack2
        r.input = getInterfacedSide
        r.setDestination(destination, priority)
        sendQueue :+= r
    }

    override def getLogisticPath(item:ItemKey, exclusions:BitSet, excludeStart:Boolean) =
    {
        LogisticPathFinder.clear()
        LogisticPathFinder.start = getRouter
        LogisticPathFinder.payload = item
        LogisticPathFinder.exclusions = exclusions
        LogisticPathFinder.excludeSource = excludeStart
        val result = LogisticPathFinder.result()
        LogisticPathFinder.clear()
        result
    }

    override def getWorld = world

    override def getActiveFreeSpace(item:ItemKey) =
    {
        val real = getInventory
        if (real == null) 0
        else real.getSpaceForItem(item)
    }
}

object TNetworkPipe
{
    var delayDelta = 0
}

abstract class AbstractNetPipe extends PayloadPipePart[NetworkPayload] with TRedstonePipe
class BasicPipePart extends AbstractNetPipe with TNetworkSubsystem
class RoutedJunctionPipePart extends AbstractNetPipe with TNetworkPipe
