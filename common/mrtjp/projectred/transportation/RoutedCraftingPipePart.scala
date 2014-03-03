package mrtjp.projectred.transportation

import codechicken.core.IGuiPacketSender
import codechicken.core.ServerUtils
import codechicken.lib.data.MCDataInput
import codechicken.lib.packet.PacketCustom
import codechicken.lib.vec.BlockCoord
import java.util.UUID
import java.util.concurrent.DelayQueue
import mrtjp.projectred.core.BasicGuiUtils
import mrtjp.projectred.core.ItemDataCard
import mrtjp.projectred.core.inventory.GhostContainer2
import mrtjp.projectred.core.inventory.GhostContainer2.ISlotController
import mrtjp.projectred.core.inventory.InvWrapper
import mrtjp.projectred.core.inventory.SimpleInventory
import mrtjp.projectred.core.utils.ItemKey
import mrtjp.projectred.core.utils.ItemKeyStack
import mrtjp.projectred.core.utils.Pair2
import mrtjp.projectred.core.utils.PostponedWorkItem
import mrtjp.projectred.transportation.ItemRoutingChip.EnumRoutingChip
import mrtjp.projectred.transportation.RequestBranchNode.{DeliveryPromise, CraftingPromise, ExcessPromise}
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.entity.player.EntityPlayerMP
import net.minecraft.inventory.Container
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.MovingObjectPosition
import scala.collection
import scala.collection.immutable

class RoutedCraftingPipePart extends RoutedJunctionPipePart with IWorldCrafter
{
    private val chipSlots = new SimpleInventory(8, "chips", 1)
    {
        override def onInventoryChanged()
        {
            super.onInventoryChanged()
            refreshChips()
        }

        override def isItemValidForSlot(i:Int, stack:ItemStack) =
            stack != null && stack.getItem.isInstanceOf[ItemRoutingChip] &&
                stack.hasTagCompound && stack.getTagCompound.hasKey("chipROM") &&
                EnumRoutingChip.getForStack(stack).isCraftingChip
    }
    private val cardSlots = new SimpleInventory(9, "links", 1)
    {
        override def isItemValidForSlot(i:Int, stack:ItemStack):Boolean =
        {
            if (ItemDataCard.hasCardData(stack))
            {
                val tag = ItemDataCard.loadData(stack, "ext_pipe")
                if (tag.hasKey("id")) return true
            }
            false
        }

        override def onInventoryChanged()
        {
            refreshExtensions()
        }
    }
    private val chips = new Array[ChipCrafting](9)

    private val manager = new DeliveryManager
    private final var excess = List[Pair2[ItemKeyStack, IWorldRequester]]() //TODO can change to List[ItemKeyStack], IWR is always null
    private final val lost = new DelayQueue[PostponedWorkItem[ItemKeyStack]]
    private final val extensionIPs = new Array[Int](9)

    var priority = 0
    private var remainingDelay = operationDelay
    private var remainingDelay2 = operationDelay2

    /**
     * Standard operation delay
     */
    private def operationDelay = 10

    /**
     * Lost items handling delay
     */
    private def operationDelay2 = 40

    protected def itemsToExtract = 1

    protected def stacksToExtract = 1

    override def read(packet:MCDataInput, switch_key:Int)
    {
        if (switch_key == 45) priority = packet.readInt
        else super.read(packet, switch_key)
    }

    def priorityUp()
    {
        val old = priority
        priority = Math.min(16, priority + 1)
        if (old != priority) sendPriorityUpdate()
    }

    def priorityDown()
    {
        val old = priority
        priority = Math.max(-16, priority - 1)
        if (old != priority) sendPriorityUpdate()
    }

    private def sendPriorityUpdate()
    {
        if (!world.isRemote) tile.getWriteStream(this).writeByte(45).writeInt(priority)
    }

    override def updateServer()
    {
        remainingDelay -= 1
        if (remainingDelay <= 0)
        {
            remainingDelay = operationDelay
            operationTick()
        }

        remainingDelay2 -= 1
        if (remainingDelay2 <= 0)
        {
            remainingDelay2 = operationDelay2
            lostHandleTick()
        }
    }

    private def operationTick()
    {
        if (!manager.hasOrders && excess.isEmpty) return

        val real = getInventory
        if (real == null)
        {
            if (manager.hasOrders) manager.dispatchFailed()
            else excess = List()
            return
        }
        val side = getInterfacedSide

        val inv = InvWrapper.wrap(real).setSlotsFromSide(side)

        val wanted = getCraftedItems
        if (wanted == null || wanted.isEmpty) return

        RouteFX.spawnType1(RouteFX.color_checkInv, 8, new BlockCoord(tile), world)
        var itemsleft = itemsToExtract
        var stacksleft = stacksToExtract

        import mrtjp.projectred.core.utils.LabelBreaks._
        label {
            while (itemsleft > 0 && stacksleft > 0 && (manager.hasOrders || !excess.isEmpty))
            {
                var nextOrder:Pair2[ItemKeyStack, IWorldRequester] = null
                var processingOrder = false
                if (manager.hasOrders)
                {
                    nextOrder = manager.peek
                    processingOrder = true
                }
                else nextOrder = excess(0)

                val keyStack = nextOrder.getValue1
                var maxToSend = Math.min(itemsleft, keyStack.stackSize)
                maxToSend = Math.min(keyStack.key.getMaxStackSize, maxToSend)
                var available = inv.extractItem(keyStack.key, maxToSend)

                if (available <= 0) break

                val key = keyStack.key
                while (available > 0)
                {
                    var numToSend = Math.min(available, key.getMaxStackSize)
                    numToSend = Math.min(numToSend, keyStack.stackSize)
                    if (numToSend == 0) break
                    stacksleft -= 1
                    itemsleft -= numToSend
                    available -= numToSend
                    val toSend = key.makeStack(numToSend)

                    if (processingOrder)
                    {
                        queueStackToSend(toSend, side, SendPriority.ACTIVE, nextOrder.getValue2.getRouter.getIPAddress)
                        manager.dispatchSuccessful(numToSend, false)

                        if (manager.hasOrders) nextOrder = manager.peek
                        else
                        {
                            processingOrder = false
                            if (!excess.isEmpty) nextOrder = excess(0)
                        }
                    }
                    else
                    {
                        removeExcess(key, numToSend)
                        queueStackToSend(toSend, side, SendPriority.WANDERING, -1)
                    }
                }
            }
        }
    }

    private def lostHandleTick()
    {
        if (lost.isEmpty) return
        var post = lost.peek()

        import mrtjp.projectred.core.utils.LabelBreaks._
        while (post != null) label
        {
            val stack = post.getItem
            var toRequest = stack.stackSize
            toRequest = Math.min(toRequest, getActiveFreeSpace(stack.key))

            if (toRequest <= 0)
            {
                lost.add(new PostponedWorkItem[ItemKeyStack](stack, 5000))
                break
            }

            val req = new RequestConsole().setDestination(this)
            req.setPulling(true).setCrafting(true).setPartials(true)
            val requested = req.makeRequest(ItemKeyStack.get(stack.key, toRequest)).requested
            if (requested < stack.stackSize)
            {
                stack.stackSize -= requested
                lost.add(new PostponedWorkItem[ItemKeyStack](stack, 5000))
            }

            post = lost.peek()
        }
    }

    override def trackedItemLost(s:ItemKeyStack)
    {
        lost.add(new PostponedWorkItem[ItemKeyStack](s, 5000))
    }

    private def removeExcess(item:ItemKey, amount:Int)
    {
        var left = amount

        excess = excess.filter(p =>
        {
            val stack = p.getValue1
            if (stack.key == item)
                if (left >= stack.stackSize)
                {
                    left -= stack.stackSize
                    false
                }
                else
                {
                    stack.stackSize -= left
                    true
                }
            true
        })
    }

    private def refreshExtensions()
    {
        for (i <- 0 until 9)
        {
            val inslot = cardSlots.getStackInSlot(i)
            if (inslot != null && ItemDataCard.hasCardData(inslot))
            {
                val data = ItemDataCard.loadData(inslot, "ext_pipe")
                if (data.hasKey("id"))
                {
                    var id:UUID = null
                    try
                    {
                        id = UUID.fromString(data.getString("id"))
                        extensionIPs(i) = RouterServices.getIPforUUID(id)
                    }
                    catch {case t:Throwable =>}
                }
            }
            else extensionIPs(i) = -1
        }
    }

    def refreshChips()
    {
        for (i <- 0 until 8)
        {
            val stack = chipSlots.getStackInSlot(i)
            val c = ItemRoutingChip.loadChipFromItemStack(stack)
            if (c.isInstanceOf[ChipCrafting])
            {
                val c2 = c.asInstanceOf[ChipCrafting]
                c2.setEnvironment(this, this, i)
                if (chips(i) != c2) chips(i) = c2
            }
        }
    }

    private def getExtensionFor(slot:Int):IWorldRequester =
    {
        if (0 until 9 contains slot) if (extensionIPs(slot) >= 0 && getRouter.canRouteTo(extensionIPs(slot)))
        {
            val r = RouterServices.getRouter(extensionIPs(slot))
            if (r != null && r.isLoaded && r.getParent.isInstanceOf[IWorldRequester])
                return r.getParent.asInstanceOf[IWorldRequester]
        }
        this
    }

    override def activate(player:EntityPlayer, hit:MovingObjectPosition, item:ItemStack):Boolean =
    {
        if (super.activate(player, hit, item)) return true
        if (item != null && item.getItem.isInstanceOf[ItemRoutingChip]) for (i <- 0 until chipSlots.getSizeInventory)
        {
            if (chipSlots.getStackInSlot(i) == null && chipSlots.isItemValidForSlot(i, item))
            {
                val chip = item.splitStack(1)
                chipSlots.setInventorySlotContents(i, chip)
                chipSlots.onInventoryChanged()
                return true
            }
        }
        openGui(player)
        true
    }

    override def onRemoved()
    {
        super.onRemoved()
        if (!world.isRemote)
        {
            chipSlots.dropContents(world, x, y, z)
            cardSlots.dropContents(world, x, y, z)
        }
    }

    def openGui(player:EntityPlayer)
    {
        if (world.isRemote) return
        ServerUtils.openSMPContainer(player.asInstanceOf[EntityPlayerMP], createContainer(player), new IGuiPacketSender
        {
            def sendPacket(player:EntityPlayerMP, windowId:Int)
            {
                val p = new PacketCustom(TransportationSPH.channel, TransportationSPH.gui_CraftingPipe_open)
                p.writeCoord(x, y, z)
                p.writeByte(windowId)
                p.writeInt(priority)
                p.sendToPlayer(player)
            }
        })
    }

    def createContainer(player:EntityPlayer):Container =
    {
        val ghost = new GhostContainer2(player.inventory)
        var s = 0
        import scala.collection.JavaConversions._
        for (p <- BasicGuiUtils.createSlotArray(20, 12, 2, 4, 20, 0))
        {
            ghost.addCustomSlot(new GhostContainer2.SlotExtended(chipSlots, s, p.getValue1, p.getValue2).setCheck(ISlotController.InventoryRulesController.instance))
            s += 1
        }
        var s2 = 0
        for (p <- BasicGuiUtils.createSlotArray(8, 108, 9, 1, 0, 0))
        {
            ghost.addCustomSlot(new GhostContainer2.SlotExtended(cardSlots, s2, p.getValue1, p.getValue2).setCheck(ISlotController.InventoryRulesController.instance))
            s2 += 1
        }
        ghost.addPlayerInventory(8, 138)
        ghost
    }

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        chipSlots.save(tag, "c")
        cardSlots.save(tag, "l")
        tag.setInteger("pri", priority)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        chipSlots.load(tag, "c")
        cardSlots.load(tag, "l")
        priority = tag.getInteger("pri")
    }

    def requestPromises(request:RequestBranchNode, existingPromises:Int)
    {
        if (excess.isEmpty) return
        val requestedItem = request.getRequestedPackage
        val jobs = getCraftedItems

        if (jobs.find(p => p.key == requestedItem).getOrElse(null) == null) return

        var remaining = 0
        for (extra <- excess) if (extra.getValue1.key == requestedItem) remaining += extra.getValue1.stackSize
        remaining -= existingPromises
        if (remaining <= 0) return

        val promise = new ExcessPromise
        promise.setUsed(true).setPackage(requestedItem).setSize(Math.min(remaining, request.getMissingCount)).setSender(this)
        request.addPromise(promise)
    }

    def deliverPromises(promise:RequestBranchNode.DeliveryPromise, requester:IWorldRequester)
    {
        if (promise.isInstanceOf[ExcessPromise]) removeExcess(promise.thePackage, promise.size)
        manager.addOrder(ItemKeyStack.get(promise.thePackage, promise.size), requester)
    }

    def getBroadcastedItems(map:collection.mutable.HashMap[ItemKey, Int]) {}

    def requestCraftPromise(item:ItemKey):CraftingPromise =
    {
        val items = getCraftedItems
        if (items == null || items.isEmpty) return null

        val r = getChipFor(item)
        if (r == null) return null

        val result = ItemKeyStack.get(r.matrix.getStackInSlot(9))
        val requesters = new Array[IWorldRequester](9)

        for (i <- 0 until 9) requesters(i) = getExtensionFor(r.extIndex(i))

        val promise = new CraftingPromise(result, this, priority)

        for (i <- 0 until 9)
        {
            val keystack = ItemKeyStack.get(r.matrix.getStackInSlot(i))
            if (keystack != null && keystack.stackSize > 0) promise.addIngredient(keystack, requesters(i))
        }

        promise
    }

    def registerExcess(promise:DeliveryPromise)
    {
        val keystack = ItemKeyStack.get(promise.thePackage, promise.size)
        excess :+= new Pair2(keystack, null.asInstanceOf[IWorldRequester])
    }

    def getCraftedItems:immutable.List[ItemKeyStack] =
    {
        var list = List[ItemKeyStack]()

        for (r <- chips) if (r != null)
        {
            val stack = r.matrix.getStackInSlot(9)
            if (stack != null) list :+= ItemKeyStack.get(stack)
        }
        list
    }

    def getChipFor(key:ItemKey):ChipCrafting =
    {
        for (r <- chips) if (r != null)
        {
            val stack = r.matrix.getStackInSlot(9)
            if (stack != null && (ItemKey.get(stack) == key)) return r
        }
        null
    }

    def getPriority = priority

    def getWorkLoad = (manager.getTotalDeliveryCount+63.0D)/64.0D

    def itemsToProcess = manager.getTotalDeliveryCount

    override def getActiveFreeSpace(item:ItemKey):Int =
    {
        if (manager.hasOrders)
        {
            val r = getChipFor(manager.peek.getValue1.key)
            if (r != null)
            {
                for (i <- 0 until 10)
                {
                    val s = r.matrix.getStackInSlot(i)
                    if (s != null && (ItemKey.get(s) == item))
                        return super.getActiveFreeSpace(item)
                }
            }
            0
        }
        else super.getActiveFreeSpace(item)
    }

    override def getType = "pr_rcrafting"
}