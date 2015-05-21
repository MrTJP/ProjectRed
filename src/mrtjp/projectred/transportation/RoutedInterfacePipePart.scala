package mrtjp.projectred.transportation

import codechicken.multipart.INeighborTileChange
import mrtjp.core.gui.{GuiLib, Slot2, NodeContainer}
import mrtjp.core.inventory.SimpleInventory
import mrtjp.core.item.{ItemKey, ItemKeyStack, ItemQueue}
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.MovingObjectPosition

import scala.collection.mutable.{Builder => MBuilder}

class RoutedInterfacePipePart extends AbstractNetPipe with TNetworkPipe with IWorldBroadcaster with INeighborTileChange
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
                stack.getTagCompound.hasKey("chipROM") &&
                RoutingChipDefs.getForStack(stack).isInterfaceChip
    }
    val chips = new Array[RoutingChip](4)

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
            for (r <- chips) if (r != null) r.onPipeBroken()
            chipSlots.dropInvContents(world, x, y, z)
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

    override def activate(player:EntityPlayer, hit:MovingObjectPosition, item:ItemStack):Boolean =
    {
        if (super.activate(player, hit, item)) return true
        if (item != null && item.getItem.isInstanceOf[ItemRoutingChip])
        {
            for (i <- 0 until chipSlots.getSizeInventory)
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

    def openGui(player:EntityPlayer)
    {
        if (world.isRemote) return
        GuiInterfacePipe.open(player, createContainer(player), _.writeCoord(x, y, z))
    }

    def createContainer(player:EntityPlayer) =
    {
        val container = new NodeContainer
        var slot = 0

        for ((x, y) <- GuiLib.createSlotGrid(24, 12, 1, 4, 0, 8))
        {
            container + new Slot2(chipSlots, slot, x, y)
            slot += 1
        }

        container.addPlayerInv(player, 8, 118)
        container
    }

    def refreshChips()
    {
        for (i <- 0 until chipSlots.getSizeInventory)
        {
            val stack = chipSlots.getStackInSlot(i)
            val c = ItemRoutingChip.loadChipFromItemStack(stack)
            if (c != null) c.setEnvironment(this, this, i)
            if (chips(i) != c) chips(i) = c
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
            return best
        }
        null
    }

    override def requestPromises(request:RequestBranchNode, existingPromises:Int)
    {
        for (r <- chips) if (r != null) r.requestPromises(request, existingPromises)
    }

    override def deliverPromises(promise:DeliveryPromise, requestor:IWorldRequester)
    {
        for (r <- chips) if (r != null) r.deliverPromises(promise, requestor)
    }

    override def trackedItemLost(s:ItemKeyStack)
    {
        for (r <- chips) if (r != null) r.trackedItemLost(s)
    }

    override def trackedItemReceived(s:ItemKeyStack)
    {
        for (r <- chips) if (r != null) r.trackedItemReceived(s)
    }

    override def getBroadcasts(col:ItemQueue)
    {
        for (r <- chips) if (r != null) r.getProvidedItems(col)
    }

    override def getBroadcastPriority =
    {
        var high = Integer.MIN_VALUE
        for (r <- chips) if (r != null)
        {
            val priority = r.getBroadcastPriority
            if (priority > high) high = priority
        }
        high
    }

    override def getWorkLoad =
    {
        var high = 0.0D
        for (r <- chips) if (r != null)
        {
            val load = r.getWorkLoad
            if (load > high) high = load
        }
        high
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
}