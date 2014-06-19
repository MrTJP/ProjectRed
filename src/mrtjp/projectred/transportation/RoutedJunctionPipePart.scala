package mrtjp.projectred.transportation

import java.util.UUID
import java.util.concurrent.PriorityBlockingQueue

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.packet.PacketCustom
import codechicken.lib.vec.BlockCoord
import codechicken.multipart.{TMultiPart, TileMultipart}
import mrtjp.projectred.api.IScrewdriver
import mrtjp.projectred.core.lib.{LabelBreaks, Pair2}
import mrtjp.projectred.core.libmc.inventory.InvWrapper
import mrtjp.projectred.core.libmc.{ItemKey, ItemKeyStack, PRLib}
import mrtjp.projectred.core.{Configurator, CoreSPH}
import mrtjp.projectred.transportation.SendPriority.SendPriority
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.{IInventory, ISidedInventory}
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.tileentity.TileEntity
import net.minecraft.util.{IIcon, MovingObjectPosition}
import net.minecraftforge.common.util.ForgeDirection

import scala.collection.JavaConversions._
import scala.collection.immutable.BitSet

object RoutedJunctionPipePart
{
    var pipes = 0
}

class RoutedJunctionPipePart extends BasicPipePart with IWorldRouter with TRouteLayer with IWorldRequester with IInventoryProvider
{
    var searchDelay =
    {
        RoutedJunctionPipePart.pipes += 1
        RoutedJunctionPipePart.pipes%Configurator.detectionFrequency
    }

    var linkMap = 0

    var router:Router = null
    var routerId:UUID = null

    val routerIDLock = new AnyRef
    var inOutSide = 0
    var needsWork = true
    var firstTick = true

    var sendQueue = Vector[RoutedPayload]()
    var transitQueue = new PriorityBlockingQueue[Pair2[RoutedPayload, Int]](10, TransitComparator)
    var swapQueue = Vector[RoutedPayload]()

    var statsReceived = 0
    var statsSent = 0
    var statsRelayed = 0

    private def getRouterId =
    {
        if (routerId == null) routerIDLock synchronized
            {
                routerId = if (router != null) router.getID else UUID.randomUUID
            }
        routerId
    }

    def getRouter:Router =
    {
        if (needsWork) return null
        if (router == null) routerIDLock synchronized
            {
                router = RouterServices.getOrCreateRouter(getRouterId, this)
            }
        router
    }

    def itemEnroute(r:RoutedPayload)
    {
        transitQueue.add(new Pair2(r, 200))
    }

    def itemArrived(r:RoutedPayload)
    {
        removeFromTransitQueue(r)
        trackedItemReceived(r.payload)

        statsReceived+=1
    }

    private def removeFromTransitQueue(r:RoutedPayload)
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

    def queueStackToSend(stack:ItemStack, dirOfExtraction:Int, priority:SendPriority, destination:Int)
    {
        val stack2 = ItemKeyStack.get(stack)
        var r = pollFromSwapQueue(stack2)
        if (r == null)
        {
            r = RoutedPayload(stack2)
            r.input = ForgeDirection.getOrientation(dirOfExtraction)
            r.setPriority(priority)
            r.setDestination(destination)
        }
        sendQueue :+= r
    }

    private def dispatchQueuedPayload(r:RoutedPayload)
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

    def queueSwapSendItem(r:RoutedPayload)
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

    def getLogisticPath(stack:ItemKey, exclusions:BitSet, excludeStart:Boolean) =
    {
        val p = new LogisticPathFinder(getRouter, stack)
        if (exclusions != null) p.setExclusions(exclusions)
        p.setExcludeSource(excludeStart).findBestResult
        p.getResult
    }

    def getSyncResponse(item:ItemKey, rival:SyncResponse):SyncResponse = null

    final override def update()
    {
        if (needsWork)
        {
            needsWork = false
            if (!world.isRemote) getRouter
            return
        }
        if (!world.isRemote) getRouter.update(world.getTotalWorldTime%Configurator.detectionFrequency == searchDelay || firstTick)
        super.update()
        firstTick = false

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

    protected def updateServer() {}

    protected def updateClient()
    {
        if (world.getTotalWorldTime%(Configurator.detectionFrequency*20) == searchDelay || firstTick)
            for (i <- 0 until 6) if ((linkMap & 1 << i) != 0)
                RouteFX.spawnType3(RouteFX.color_blink, 1, i, getCoords, world)
    }

    def refreshState:Boolean =
    {
        if (world.isRemote) return false
        var link = 0
        for (s <- 0 until 6) if (getRouter.LSAConnectionExists(s)) link |= 1<<s
        if (getRouter.hasUsableController) link |= 1<<6
        if (linkMap != link)
        {
            linkMap = link
            sendLinkMapUpdate()
            return true
        }
        false
    }

    def getContainer = this

    override def endReached(r:RoutedPayload)
    {
        if (!world.isRemote) if (!maskConnects(r.output.ordinal) || !passToNextPipe(r))
        {
            val bc = new BlockCoord(tile).offset(r.output.ordinal)
            val t = PRLib.getTileEntity(world, bc, classOf[TileEntity])
            val state = LSPathFinder.getLinkState(t)

            if (state != null && t.isInstanceOf[IInventory])
            {
                val dest = state.getLink(t)
                val inv = t.asInstanceOf[IInventory]
                if (dest.isInstanceOf[TileMultipart])
                {
                    val part = (dest.asInstanceOf[TileMultipart]).partMap(6)
                    if (part.isInstanceOf[RoutedJunctionPipePart])
                    {
                        val pipe = part.asInstanceOf[RoutedJunctionPipePart]
                        val w = InvWrapper.wrap(inv).setSlotsFromSide(r.output.getOpposite.ordinal)
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
                val w = InvWrapper.wrap(inv).setSlotsFromSide(r.output.getOpposite.ordinal)
                r.payload.stackSize -= w.injectItem(r.payload.makeStack, true)
            }
            if (r.payload.stackSize > 0) bounceStack(r)
        }
    }

    override def read(packet:MCDataInput, key:Int) = key match
    {
        case 5 => handleLinkMap(packet)
        case 6 =>
            inOutSide = packet.readUByte
            tile.markRender()
        case _ => super.read(packet, key)
    }

    def sendLinkMapUpdate()
    {
        getWriteStreamOf(5).writeByte(linkMap)
    }

    def sendOrientUpdate()
    {
        getWriteStreamOf(6).writeByte(inOutSide)
    }

    private def handleLinkMap(packet:MCDataInput)
    {
        val old = linkMap
        linkMap = packet.readUByte
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
        RoutedJunctionPipePart.pipes = Math.max(RoutedJunctionPipePart.pipes-1, 0)
        val r = getRouter
        if (r != null) r.decommission()
    }

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setString("rid", getRouterId.toString)
        tag.setByte("io", inOutSide.asInstanceOf[Byte])
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
        inOutSide = tag.getByte("io")

        statsSent = tag.getInteger("sent")
        statsReceived = tag.getInteger("rec")
        statsRelayed = tag.getInteger("relay")
    }

    override def writeDesc(packet:MCDataOutput)
    {
        super.writeDesc(packet)
        packet.writeByte(linkMap)
        packet.writeByte(inOutSide)
        packet.writeLong(getRouterId.getMostSignificantBits)
        packet.writeLong(getRouterId.getLeastSignificantBits)
    }

    override def readDesc(packet:MCDataInput)
    {
        super.readDesc(packet)
        linkMap = packet.readUByte
        inOutSide = packet.readUByte
        val mostSigBits = packet.readLong
        val leastSigBits = packet.readLong
        routerIDLock synchronized
            {
                routerId = new UUID(mostSigBits, leastSigBits)
            }
    }

    override def onNeighborChanged()
    {
        super.onNeighborChanged()
        shiftOrientation(false)
    }

    override def onPartChanged(p:TMultiPart)
    {
        super.onPartChanged(p)
        shiftOrientation(false)
    }

    override def onAdded()
    {
        super.onAdded()
        shiftOrientation(false)
    }

    override def discoverStraightOverride(s:Int):Boolean =
    {
        PRLib.getTileEntity(world, posOfStraight(s), classOf[TileEntity]) match
        {
            case sinv:ISidedInventory => !sinv.getAccessibleSlotsFromSide(s^1).isEmpty
            case inv:IInventory => true
            case pow:TControllerLayer if s == 0 => true
            case _ => false
        }
    }

    override def activate(player:EntityPlayer, hit:MovingObjectPosition, item:ItemStack):Boolean =
    {
        if (super.activate(player, hit, item)) return true

        if (item != null && item.getItem.isInstanceOf[ItemRouterUtility])
        {
            if (!world.isRemote)
            {
                val s = "/#f"+"route statistics: "+
                        "\nreceived: "+statsReceived+
                        "\nsent: "+statsSent+
                        "\nrelayed: "+statsRelayed+
                        "\n\nroute table size: "+getRouter.getRouteTable.foldLeft(0)((b, v) => if (v != null) b+1 else b)
                val packet = new PacketCustom(CoreSPH.channel, CoreSPH.messagePacket)
                packet.writeDouble(x+0.0D)
                packet.writeDouble(y+0.5D)
                packet.writeDouble(z+0.0D)
                packet.writeString(s)
                packet.sendToPlayer(player)
            }
            return true
        }

        if (item != null && item.getItem.isInstanceOf[IScrewdriver])
        {
            if (!world.isRemote)
            {
                shiftOrientation(true)
                item.getItem.asInstanceOf[IScrewdriver].damageScrewdriver(world, player)
            }
            return true
        }
        false
    }

    def shiftOrientation(force:Boolean)
    {
        if (world.isRemote) return
        val invalid = force || !maskConnects(inOutSide) ||
            PRLib.getTileEntity(world, new BlockCoord(tile).offset(inOutSide), classOf[IInventory]) == null
        if (!invalid) return
        var found = false
        val oldSide = inOutSide

        import mrtjp.projectred.core.lib.LabelBreaks._
        label {
            for (i <- 0 until 6)
            {
                inOutSide = (inOutSide+1)%6
                if (maskConnects(inOutSide))
                {
                    val bc = new BlockCoord(tile).offset(inOutSide)
                    val t = PRLib.getTileEntity(world, bc, classOf[TileEntity])
                    if (t.isInstanceOf[IInventory])
                    {
                        found = true
                        break()
                    }
                }
            }
        }

        if (!found) inOutSide = -1
        if (oldSide != inOutSide) sendOrientUpdate()
    }

    def getInventory =
    {
        if (0 until 6 contains inOutSide) InvWrapper.getInventory(world, new BlockCoord(tile).offset(inOutSide))
        else null
    }

    def getInterfacedSide = if (inOutSide < 0 || inOutSide > 5) -1 else inOutSide^1

    override def getIcon(side:Int):IIcon =
    {
        val array = PipeDefs.ROUTEDJUNCTION.sprites
        var ind = if (side == inOutSide) 2 else 0
        if ((linkMap&1<<6) != 0) ind += 4
        if ((linkMap&1<<side) != 0) array(1+ind)
        else array(2+ind)
    }

    override def resolveDestination(r:RoutedPayload)
    {
        if (needsWork) return
        var color = -1
        r.output = ForgeDirection.UNKNOWN

        def reRoute(r:RoutedPayload)
        {
            r.resetTrip
            val f = new LogisticPathFinder(getRouter, r.payload.key).setExclusions(r.travelLog).findBestResult
            if (f.getResult != null)
            {
                r.setDestination(f.getResult.responder).setPriority(f.getResult.priority)
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
            if (r.output == ForgeDirection.UNKNOWN) reRoute(r)
            else
            {
                color = RouteFX.color_receive
                r.hasArrived = true
                itemArrived(r)
            }
            r.travelLog += getRouter.getIPAddress
        }

        // Relay item
        if (r.output == ForgeDirection.UNKNOWN)
        {
            r.output = getRouter.getDirection(r.destinationIP, r.payload.key, r.priority)
            color = RouteFX.color_relay
        }

        // Set to wander, clear log for re-push
        if (r.output == ForgeDirection.UNKNOWN)
        {
            super.resolveDestination(r)
            r.resetTrip
            r.travelLog = BitSet()
            color = RouteFX.color_routeLost
        }

        if (color == RouteFX.color_relay) statsRelayed += 1
        RouteFX.spawnType1(color, 8, new BlockCoord(tile), world)
        adjustSpeed(r)
    }

    def getDirForIncomingItem(r:RoutedPayload) = ForgeDirection.getOrientation(inOutSide)

    override def adjustSpeed(r:RoutedPayload)
    {
        r.speed = r.priority.boost
    }

    def trackedItemLost(s:ItemKeyStack) {}
    def trackedItemReceived(s:ItemKeyStack) {}

    def getActiveFreeSpace(item:ItemKey) =
    {
        val real = getInventory
        if (real == null) 0
        else InvWrapper.wrap(real).setSlotsFromSide(getInterfacedSide).getSpaceForItem(item)
    }

    def getWorldRouter = this
    def getBroadcaster = this match
    {
        case b:IWorldBroadcaster => b
        case _ => null
    }
    def getRequester = this

    def getWorld = world
    def getCoords = new BlockCoord(tile)
}

object TransitComparator extends Ordering[Pair2[RoutedPayload, Int]]
{
    def compare(a:Pair2[RoutedPayload, Int], b:Pair2[RoutedPayload, Int]) =
    {
        var c = b.get2-a.get2
        if (c == 0) c = b.get1.payload.compareTo(a.get1.payload)
        c
    }
}
