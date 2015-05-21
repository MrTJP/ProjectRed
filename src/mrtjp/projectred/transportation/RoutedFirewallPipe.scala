package mrtjp.projectred.transportation

import codechicken.lib.data.{MCDataOutput, MCDataInput}
import mrtjp.core.gui.{GuiLib, Slot2, NodeContainer}
import mrtjp.core.item.ItemKey
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.MovingObjectPosition
import mrtjp.core.inventory.SimpleInventory

class RoutedFirewallPipe extends AbstractNetPipe with TNetworkPipe
{
    var filt = new SimpleInventory(7*5, "filt", 1)
    {
        override def markDirty(){buildItemSet()}
    }
    var filtExclude = true

    var allowRoute = true
    var allowBroadcast = true
    var allowCrafting = true
    var allowController = true

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        filt.saveInv(tag)
        tag.setBoolean("excl", filtExclude)
        tag.setBoolean("route", allowRoute)
        tag.setBoolean("broad", allowBroadcast)
        tag.setBoolean("craft", allowCrafting)
        tag.setBoolean("cont", allowController)
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
        allowController = tag.getBoolean("cont")
    }

    def sendOptUpdate()
    {
        writeInfo(getWriteStreamOf(7))
    }
    private def writeInfo(out:MCDataOutput)
    {
        out.writeBoolean(filtExclude).writeBoolean(allowRoute)
            .writeBoolean(allowBroadcast).writeBoolean(allowCrafting)
            .writeBoolean(allowController)
    }

    override def read(packet:MCDataInput, key:Int) = key match
    {
        case 7 =>
            filtExclude = packet.readBoolean()
            allowRoute = packet.readBoolean()
            allowBroadcast = packet.readBoolean()
            allowCrafting = packet.readBoolean()
            allowController = packet.readBoolean()
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
    {
        val cont = new NodeContainer

        var s = 0
        for ((x, y) <- GuiLib.createSlotGrid(8, 8, 7, 5, 0, 0))
        {
            cont + new Slot2(filt, s, x, y).setGhosting(true)
            s += 1
        }
        cont.addPlayerInv(player, 8, 120).setShift(false)
    }

    override def networkFilter =
        (if (allowRoute) 0x1 else 0)|(if (allowBroadcast) 0x2 else 0)|(if (allowCrafting) 0x4 else 0)

    override def filteredItems = itemset

    override def itemsExclude = filtExclude

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