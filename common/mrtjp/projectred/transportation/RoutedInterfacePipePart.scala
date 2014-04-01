package mrtjp.projectred.transportation

import codechicken.core.IGuiPacketSender
import codechicken.core.ServerUtils
import codechicken.lib.packet.PacketCustom
import codechicken.multipart.INeighborTileChange
import mrtjp.projectred.core.BasicGuiUtils
import mrtjp.projectred.core.inventory.SimpleInventory
import mrtjp.projectred.core.inventory.SpecialContainer
import mrtjp.projectred.core.inventory.SpecialContainer.ISlotController
import mrtjp.projectred.core.utils.ItemKey
import mrtjp.projectred.core.utils.ItemKeyStack
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.entity.player.EntityPlayerMP
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.MovingObjectPosition

class RoutedInterfacePipePart extends RoutedJunctionPipePart with IWorldBroadcaster with INeighborTileChange
{
    val chipSlots = new SimpleInventory(4, "chips", 1)
    {
        override def onInventoryChanged()
        {
            super.onInventoryChanged()
            refreshChips()
        }

        override def isItemValidForSlot(i:Int, stack:ItemStack) =
            stack != null &&
                stack.getItem.isInstanceOf[ItemRoutingChip] &&
                stack.hasTagCompound &&
                stack.getTagCompound.hasKey("chipROM") &&
                EnumRoutingChip.getForStack(stack).isInterfaceChip
    }
    val chips = new Array[RoutingChipset](4)

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        chipSlots.save(tag)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        chipSlots.load(tag)
    }

    override def onRemoved()
    {
        super.onRemoved()
        if (!world.isRemote)
        {
            for (r <- chips) if (r != null) r.onPipeBroken()
            chipSlots.dropContents(world, x, y, z)
        }
    }

    override def updateServer()
    {
        super.updateServer()
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
                    chipSlots.onInventoryChanged()
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
        ServerUtils.openSMPContainer(player.asInstanceOf[EntityPlayerMP], createContainer(player), new IGuiPacketSender
        {
            def sendPacket(player:EntityPlayerMP, windowId:Int)
            {
                val p = new PacketCustom(TransportationSPH.channel, TransportationSPH.gui_InterfacePipe_open)
                p.writeCoord(x, y, z)
                p.writeByte(windowId)
                p.sendToPlayer(player)
            }
        })
    }

    def createContainer(player:EntityPlayer) =
    {
        val ghost = new SpecialContainer(player.inventory)
        val sc = ISlotController.InventoryRulesController.instance

        var slot = 0
        import scala.collection.JavaConversions._
        for (p <- BasicGuiUtils.createSlotArray(24, 12, 1, 4, 0, 8))
        {
            ghost.addCustomSlot(new SpecialContainer.SlotExtended(chipSlots, slot, p.getValue1, p.getValue2).setCheck(sc))
            slot += 1
        }

        ghost.addPlayerInventory(8, 118)
        ghost
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

    def requestPromises(request:RequestBranchNode, existingPromises:Int)
    {
        for (r <- chips) if (r != null) r.requestPromises(request, existingPromises)
    }

    def deliverPromises(promise:DeliveryPromise, requestor:IWorldRequester)
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

    def getBroadcastedItems(map:collection.mutable.HashMap[ItemKey, Int])
    {
        for (r <- chips) if (r != null) r.getProvidedItems(map)
    }

    def getBroadcastPriority =
    {
        var high = Integer.MIN_VALUE
        for (r <- chips) if (r != null)
        {
            val priority = r.getBroadcastPriority
            if (priority > high) high = priority
        }
        high
    }

    def getWorkLoad =
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