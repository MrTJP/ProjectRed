package mrtjp.projectred.transportation

import java.util.UUID
import java.util.concurrent.DelayQueue

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.vec.BlockCoord
import mrtjp.core.gui.{GuiLib, Slot2, WidgetContainer}
import mrtjp.core.inventory.{InvWrapper, SimpleInventory}
import mrtjp.core.item.{ItemKey, ItemKeyStack}
import mrtjp.core.util.{Pair2, PostponedWorkItem}
import mrtjp.projectred.core.ItemDataCard
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.Container
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.MovingObjectPosition

class RoutedCraftingPipePart extends BasicPipeAbstraction with TNetworkPipe with IWorldCrafter
{
    private val chipSlots = new SimpleInventory(8, "chips", 1)
    {
        override def markDirty()
        {
            chipsNeedRefresh = true
        }

        override def isItemValidForSlot(i:Int, stack:ItemStack) =
            stack != null && stack.getItem.isInstanceOf[ItemRoutingChip] &&
                stack.hasTagCompound && stack.getTagCompound.hasKey("chipROM") &&
                RoutingChipDefs.getForStack(stack).isCraftingChip
    }
    private val cardSlots = new SimpleInventory(9, "links", 1)
    {
        override def isItemValidForSlot(i:Int, stack:ItemStack):Boolean =
        {
            if (ItemDataCard.hasData(stack))
            {
                val id = ItemDataCard.getData(stack, "extension")
                return !id.isEmpty
            }
            false
        }

        override def markDirty()
        {
            extensionsNeedRefresh = true
        }
    }
    private val chips = new Array[ChipCrafting](9)

    private val manager = new DeliveryManager
    private var excess = Vector[Pair2[ItemKeyStack, IWorldRequester]]() //TODO can change to Vector[ItemKeyStack], IWR is always null
    private val lost = new DelayQueue[PostponedWorkItem[ItemKeyStack]]

    private var chipsNeedRefresh = true
    private var extensionsNeedRefresh = true

    private val extensionIPs = new Array[Int](9)

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
        if (!world.isRemote) getWriteStreamOf(7).writeInt(priority)
    }

    override def read(packet:MCDataInput, key:Int) = key match
    {
        case 7 => priority = packet.readInt()
        case _ => super.read(packet, key)
    }

    override def readDesc(packet:MCDataInput)
    {
        super.readDesc(packet)
        priority = packet.readInt()
    }

    override def writeDesc(packet:MCDataOutput)
    {
        super.writeDesc(packet)
        packet.writeInt(priority)
    }

    override def updateServer()
    {
        super.updateServer()

        if (chipsNeedRefresh)
        {
            chipsNeedRefresh = false
            refreshChips()
        }

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
            else excess = Vector()
            return
        }
        val side = getInterfacedSide

        val inv = InvWrapper.wrap(real).setSlotsFromSide(side)

        val wanted = getCraftedItems
        if (wanted == null || wanted.isEmpty) return

        RouteFX.spawnType1(RouteFX.color_checkInv, 8, new BlockCoord(tile), world)
        var itemsleft = itemsToExtract
        var stacksleft = stacksToExtract

        import scala.util.control.Breaks._
        breakable {
            while (itemsleft > 0 && stacksleft > 0 && (manager.hasOrders || excess.nonEmpty))
            {
                var processingOrder = false
                var nextOrder:Pair2[ItemKeyStack, IWorldRequester] = null
                if (manager.hasOrders)
                {
                    nextOrder = manager.peek
                    processingOrder = true
                }
                else nextOrder = excess(0)

                val keyStack = nextOrder.get1
                var maxToSend = Math.min(itemsleft, keyStack.stackSize)
                maxToSend = Math.min(keyStack.key.getMaxStackSize, maxToSend)

                var available = inv.extractItem(keyStack.key, maxToSend)
                if (available <= 0) break()

                val key = keyStack.key
                while (available > 0)
                {
                    var numToSend = Math.min(available, key.getMaxStackSize)
                    numToSend = Math.min(numToSend, keyStack.stackSize)
                    if (numToSend == 0) break()
                    stacksleft -= 1
                    itemsleft -= numToSend
                    available -= numToSend
                    val toSend = key.makeStack(numToSend)

                    if (processingOrder)
                    {
                        queueStackToSend(toSend, side, Priorities.ACTIVEC, nextOrder.get2.getRouter.getIPAddress)
                        manager.dispatchSuccessful(numToSend, false)

                        if (manager.hasOrders) nextOrder = manager.peek
                        else
                        {
                            processingOrder = false
                            if (excess.nonEmpty) nextOrder = excess(0)
                        }
                    }
                    else
                    {
                        removeExcess(key, numToSend)
                        queueStackToSend(toSend, side, Priorities.WANDERING, -1)
                    }
                }
            }
        }
    }

    private def lostHandleTick()
    {
        if (lost.isEmpty) return
        var post:PostponedWorkItem[ItemKeyStack] = null

        import scala.util.control.Breaks._
        while ({post = lost.poll(); post} != null) breakable
        {
            val stack = post.getItem
            var toRequest = stack.stackSize
            toRequest = Math.min(toRequest, getActiveFreeSpace(stack.key))

            if (toRequest <= 0)
            {
                lost.add(new PostponedWorkItem[ItemKeyStack](stack, 5000))
                break()
            }

            val req = new RequestConsole(RequestFlags.full).setDestination(this)
            val requested = req.makeRequest(ItemKeyStack.get(stack.key, toRequest)).requested
            if (requested < stack.stackSize)
            {
                stack.stackSize -= requested
                lost.add(new PostponedWorkItem[ItemKeyStack](stack, 5000))
            }
        }
    }

    override def trackedItemLost(s:ItemKeyStack)
    {
        lost.add(new PostponedWorkItem[ItemKeyStack](s, 5000))
    }

    private def removeExcess(item:ItemKey, amount:Int)
    {
        var left = amount
        excess.forall(p =>
        {
            val stack = p.get1
            if (stack.key == item)
                if (left >= stack.stackSize)
                {
                    left -= stack.stackSize
                    stack.stackSize = 0
                    true
                }
                else
                {
                    stack.stackSize -= left
                    false
                }
            true
        })
        excess = excess.filter(_.get1.stackSize > 0)
    }

    private def refreshExtensions()
    {
        for (i <- 0 until 9)
        {
            val inslot = cardSlots.getStackInSlot(i)
            if (inslot != null && ItemDataCard.hasData(inslot))
            {
                val data = ItemDataCard.getData(inslot, "extension")
                if (!data.isEmpty) try
                {
                    val id = UUID.fromString(data)
                    extensionIPs(i) = RouterServices.getIPforUUID(id)
                }
                catch {case t:Throwable =>}
            }
            else extensionIPs(i) = -1
        }
    }

    def refreshChips()
    {
        for (i <- 0 until 8)
        {
            val stack = chipSlots.getStackInSlot(i)
            ItemRoutingChip.loadChipFromItemStack(stack) match
            {
                case c2:ChipCrafting =>
                    c2.setEnvironment(this, this, i)
                    if (chips(i) != c2) chips(i) = c2
                case _ =>
            }
        }
    }

    def getExtensionFor(slot:Int):IWorldRequester =
    {
        if (extensionsNeedRefresh)
        {
            refreshExtensions()
            extensionsNeedRefresh = false
        }

        if (0 until 9 contains slot) if (extensionIPs(slot) >= 0 &&
            getRouter.isInNetwork(extensionIPs(slot)))
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
                chipSlots.markDirty()
                return true
            }
        }
        if (!player.isSneaking)
        {
            openGui(player)
            true
        }
        else false
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
        GuiCraftingPipe.open(player, createContainer(player), _.writeCoord(x, y, z).writeInt(priority))
    }

    def createContainer(player:EntityPlayer):Container =
    {
        val container = new WidgetContainer
        var s = 0
        for ((x, y) <- GuiLib.createSlotGrid(20, 12, 2, 4, 20, 0))
        {
            container + new Slot2(chipSlots, s, x, y)
            s += 1
        }
        var s2 = 0
        for ((x, y) <- GuiLib.createSlotGrid(8, 108, 9, 1, 0, 0))
        {
            container + new Slot2(cardSlots, s2, x, y)
            s2 += 1
        }
        container.addPlayerInv(player, 8, 138)
        container
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

        if (!jobs.exists(_.key == requestedItem)) return

        var remaining = 0
        for (extra <- excess) if (extra.get1.key == requestedItem) remaining += extra.get1.stackSize
        remaining -= existingPromises
        if (remaining <= 0) return

        request.addPromise(new DeliveryPromise(requestedItem,
            Math.min(remaining, request.getMissingCount), this, true, true))
    }

    def deliverPromises(promise:DeliveryPromise, requester:IWorldRequester)
    {
        if (promise.isExcess) removeExcess(promise.item, promise.size)
        manager.addOrder(ItemKeyStack.get(promise.item, promise.size), requester)
    }

    def buildCraftPromises(item:ItemKey) =
    {
        val jobs = Vector.newBuilder[CraftingPromise]
        for (r <- chips) if (r != null)
        {
            val p = r.buildCraftPromise(item, this)
            if (p != null) jobs += p
        }
        jobs.result()
    }

    def registerExcess(promise:DeliveryPromise)
    {
        val keystack = ItemKeyStack.get(promise.item, promise.size)
        excess :+= new Pair2(keystack, null.asInstanceOf[IWorldRequester])
    }

    def getCraftedItems:Vector[ItemKeyStack] =
    {
        var b = Vector.newBuilder[ItemKeyStack]
        for (r <- chips) if (r != null)
        {
            val s = r.getCraftedItem
            if (s != null) b += s
        }
        b.result()
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

    def getBroadcastPriority = priority

    def getWorkLoad = (manager.getTotalDeliveryCount+63.0D)/64.0D

    def itemsToProcess = manager.getTotalDeliveryCount

    override def getActiveFreeSpace(item:ItemKey):Int =
    {
        if (manager.hasOrders)
        {
            val r = getChipFor(manager.peek.get1.key)
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
}