package mrtjp.projectred.transportation

import codechicken.lib.raytracer.CuboidRayTraceResult
import codechicken.multipart.INeighborTileChangePart
import mrtjp.core.gui.{GuiLib, NodeContainer, Slot3}
import mrtjp.core.inventory.SimpleInventory
import mrtjp.core.item.{ItemKey, ItemKeyStack, ItemQueue}
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.EnumHand

class RoutedInterfacePipePart extends AbstractNetPipe with TNetworkPipe with IWorldBroadcaster with IWorldCrafter with INeighborTileChangePart
{
    val chipSlots = new SimpleInventory(4, "chips", 1)
    {
        override def markDirty()
        {
            chipsNeedRefresh = true
        }

        override def isItemValidForSlot(i:Int, stack:ItemStack) =
            stack != null &&
                stack.getItem.isInstanceOf[ItemRoutingChip] &&
                stack.hasTagCompound &&
                stack.getTagCompound.hasKey("chipROM")
    }

    val chips = new Array[RoutingChip](4)
    val chipStacks = new Array[ItemKey](4)

    private var chipsNeedRefresh = true

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        chipSlots.saveInv(tag)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        chipSlots.loadInv(tag)
    }

    override def onRemoved()
    {
        super.onRemoved()
        if (!world.isRemote)
        {
            for (r <- chips) if (r != null) r.onRemoved()
            chipSlots.dropInvContents(world, pos)
        }
    }

    override def updateServer()
    {
        super.updateServer()

        if (chipsNeedRefresh)
        {
            chipsNeedRefresh = false
            refreshChips()
        }

        for (s <- chips) if (s != null) s.update()
    }


    override def activate(player:EntityPlayer, hit:CuboidRayTraceResult, item:ItemStack, hand:EnumHand):Boolean =
    {
        if (super.activate(player, hit, item, hand)) return true
        if (item != null && item.getItem.isInstanceOf[ItemRoutingChip])
        {
            for (i <- 0 until chipSlots.getSizeInventory)
                if (chipSlots.getStackInSlot(i) == null && chipSlots.isItemValidForSlot(i, item))
                {
                    val chip = item.splitStack(1)
                    chipSlots.setInventorySlotContents(i, chip)
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

    def openGui(player:EntityPlayer)
    {
        if (world.isRemote) return
        GuiInterfacePipe.open(player, createContainer(player), _.writePos(pos))
    }

    def createContainer(player:EntityPlayer) = new ContainerInterfacePipe(this, player)

    def refreshChips()
    {
        for (i <- 0 until chipSlots.getSizeInventory)
        {
            val oldKey = chipStacks(i)
            val newStack = chipSlots.getStackInSlot(i)
            val newKey = if (newStack != null) ItemKey.get(newStack) else null

            if (newKey != oldKey)
            {
                val oldChip = chips(i)
                val newChip = if (newStack != null && ItemRoutingChip.isValidChip(newStack))
                    ItemRoutingChip.loadChipFromItemStack(newStack) else null

                if (oldChip != null)
                {
                    oldChip.onRemoved()
                    chips(i) = null
                }

                if (newChip != null)
                {
                    newChip.setEnvironment(this, this, i)
                    chips(i) = newChip
                    newChip.onAdded()
                }
            }
        }

        for (i <- 0 until chipSlots.getSizeInventory)
        {
            val s = chipSlots.getStackInSlot(i)
            chipStacks(i) = if (s != null) ItemKey.get(s) else null
        }
    }

    override def getSyncResponse(item:ItemKey, rival:SyncResponse):SyncResponse =
    {
        var best = rival
        var found = false
        for (r <- chips) if (r != null)
        {
            val response = r.getSyncResponse(item, best)
            if (response != null) if (response.isPreferredOver(best))
            {
                best = response
                found = true
            }
        }
        if (found)
        {
            best.itemCount -= countInTransit(item)
            if (best.itemCount > 0) return best
        }
        null
    }

    override def requestPromise(request:RequestBranchNode, existingPromises:Int)
    {
        for (r <- chips) if (r != null) r.requestPromise(request, existingPromises)
    }

    override def deliverPromise(promise:DeliveryPromise, requestor:IWorldRequester)
    {
        for (r <- chips) if (r != null) r.deliverPromise(promise, requestor)
    }

    def postEventToChips(event:NetworkEvent)
    {
        val it = chips.filter(_ != null).iterator
        while (it.hasNext && !event.isCanceled)
            it.next().onEventReceived(event)
    }


    override def postNetworkEvent(event:NetworkEvent)
    {
        super.postNetworkEvent(event:NetworkEvent)
        postEventToChips(event)
    }

//    override def itemReceived(stack:ItemKeyStack)
//    {
//        super.itemReceived(stack)
//        postEventToChips(new ItemReceivedEvent(stack.key, stack.stackSize))
//    }
//
//    override def itemLost(stack:ItemKeyStack)
//    {
//        super.itemLost(stack)
//        postEventToChips(new ItemLostEvent(stack.key, stack.stackSize))
//    }

    override def getBroadcasts(col:ItemQueue)
    {
        for (r <- chips) if (r != null) r.getBroadcasts(col)
    }

    override def getBroadcastPriority =
    {
        val all = chips.filter(_ != null).map(_.getBroadcastPriority)
        if (all.isEmpty) Integer.MIN_VALUE else all.max
    }

    override def getWorkLoad =
    {
        val all = chips.filter(_ != null).map(_.getWorkLoad)
        if (all.isEmpty) 0 else all.max
    }


    override def onNeighborTileChanged(side:Int, weak:Boolean)
    {
        for (r <- chips) if (r != null) r.onNeighborTileChanged(side, weak)
    }

    override def weakTileChanges():Boolean =
    {
        for (r <- chips) if (r != null) if (r.weakTileChanges) return true
        false
    }

    override def requestCraftPromise(request:RequestBranchNode) =
    {
        val b = Seq.newBuilder[CraftingPromise]
        for (r <- chips) if (r != null)
        {
            val p = r.requestCraftPromise(request)
            if (p != null) b += p
        }
        b.result()
    }

    override def registerExcess(promise:DeliveryPromise)
    {
        for (r <- chips) if (r != null)
            r.registerExcess(promise)
    }

    override def getCraftedItems =
    {
        var b = Seq.newBuilder[ItemKeyStack]
        for (r <- chips) if (r != null)
        {
            val s = r.getCraftedItem
            if (s != null) b += s
        }
        b.result()
    }

    override def itemsToProcess =
        chips.filterNot(_ == null).foldLeft(0){(count, r) => r.getProcessingItems+count}
}

class ContainerInterfacePipe(pipe:RoutedInterfacePipePart, p:EntityPlayer) extends NodeContainer
{
    {
        for (((x, y), i) <- GuiLib.createSlotGrid(24, 12, 1, 4, 0, 8).zipWithIndex)
            addSlotToContainer(new Slot3(pipe.chipSlots, i, x, y))

        addPlayerInv(p, 8, 118)
    }
}