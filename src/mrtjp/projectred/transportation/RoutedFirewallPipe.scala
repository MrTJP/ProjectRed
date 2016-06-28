package mrtjp.projectred.transportation

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import mrtjp.core.gui.{GuiLib, NodeContainer, Slot3}
import mrtjp.core.inventory.SimpleInventory
import mrtjp.core.item.ItemKey
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.MovingObjectPosition

class RoutedFirewallPipe extends AbstractNetPipe with TNetworkPipe
{
    var filt = new SimpleInventory(16, "filt", 1)
    {
        override def markDirty(){buildItemSet()}
    }
    var filtExclude = true

    var allowRoute = true
    var allowBroadcast = true
    var allowCrafting = true

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        filt.saveInv(tag)
        tag.setBoolean("excl", filtExclude)
        tag.setBoolean("route", allowRoute)
        tag.setBoolean("broad", allowBroadcast)
        tag.setBoolean("craft", allowCrafting)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        filt.loadInv(tag)
        buildItemSet()
        filtExclude = tag.getBoolean("excl")
        allowRoute = tag.getBoolean("route")
        allowBroadcast = tag.getBoolean("broad")
        allowCrafting = tag.getBoolean("craft")
    }

    def sendOptUpdate()
    {
        writeInfo(getWriteStreamOf(7))
    }

    private def writeInfo(out:MCDataOutput)
    {
        out.writeBoolean(filtExclude).writeBoolean(allowRoute)
            .writeBoolean(allowBroadcast).writeBoolean(allowCrafting)
    }

    override def read(packet:MCDataInput, key:Int) = key match
    {
        case 7 =>
            filtExclude = packet.readBoolean()
            allowRoute = packet.readBoolean()
            allowBroadcast = packet.readBoolean()
            allowCrafting = packet.readBoolean()
        case _ => super.read(packet, key)
    }

    override def activate(player:EntityPlayer, hit:MovingObjectPosition, item:ItemStack):Boolean =
    {
        if (super.activate(player, hit, item)) return true
        if (!player.isSneaking)
        {
            openGui(player)
            true
        }
        else false
    }

    private def openGui(player:EntityPlayer)
    {
        if (world.isRemote) return
        GuiFirewallPipe.open(player, createContainer(player), p =>
        {
            p.writeCoord(x, y, z)
            writeInfo(p)
        })
    }

    def createContainer(player:EntityPlayer) =
        new ContainerFirewallPipe(this, player)

    override def networkFilter =
        (if (allowRoute) 0x1 else 0)|(if (allowBroadcast) 0x2 else 0)|(if (allowCrafting) 0x4 else 0)

    override def itemsExclude = filtExclude

    override def filteredItems = itemset

    var itemset = Set[ItemKey]()
    def buildItemSet()
    {
        itemset = Set[ItemKey]()
        for (i <- 0 until filt.getSizeInventory)
        {
            val inslot = filt.getStackInSlot(i)
            if (inslot != null) itemset += ItemKey.get(inslot)
        }
    }
}

class ContainerFirewallPipe(pipe:RoutedFirewallPipe, player:EntityPlayer) extends NodeContainer
{
    {
        for (((x, y), i) <- GuiLib.createSlotGrid(26, 17, 4, 4, 0, 0).zipWithIndex)
        {
            val s = new Slot3(pipe.filt, i, x, y)
            s.phantomSlot = true
            addSlotToContainer(s)
        }
        addPlayerInv(player, 8, 102)
    }
    
    override def slotClick(id:Int, mouse:Int, shift:Int, player:EntityPlayer):ItemStack =
    {
        if (shift == 0 || shift == 5)
        {
            return super.slotClick(id, mouse, shift, player)
        }
        null
    }
    
    override def doMerge(stack:ItemStack, from:Int):Boolean =
    {
        if (16 to 24 contains from) //hotbar
        {
            if (tryMergeItemStack(stack, 16, 25, false)) return true
        }
        else if (25 to 52 contains from) //inv
        {
            if (tryMergeItemStack(stack, 25, 53, false)) return true
        }
        false
    }
}
