package mrtjp.projectred.transportation

import codechicken.core.{IGuiPacketSender, ServerUtils}
import codechicken.lib.data.{MCDataOutput, MCDataInput}
import codechicken.lib.packet.PacketCustom
import mrtjp.projectred.core.BasicGuiUtils
import mrtjp.projectred.core.inventory.SpecialContainer.SlotExtended
import mrtjp.projectred.core.inventory.{SpecialContainer, SimpleInventory}
import net.minecraft.entity.player.{EntityPlayerMP, EntityPlayer}
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.MovingObjectPosition
import mrtjp.projectred.core.utils.ItemKey

class RoutedFirewallPipe extends RoutedJunctionPipePart
{
    var filt = new SimpleInventory(7*5, "filt", 1)
    {
        override def onInventoryChanged()
        {
            buildItemSet()
        }
    }
    var filtExclude = true

    var allowRoute = true
    var allowBroadcast = true
    var allowCrafting = true
    var allowController = true

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        filt.save(tag)
        tag.setBoolean("excl", filtExclude)
        tag.setBoolean("route", allowRoute)
        tag.setBoolean("broad", allowBroadcast)
        tag.setBoolean("craft", allowCrafting)
        tag.setBoolean("cont", allowController)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        filt.load(tag)
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
        ServerUtils.openSMPContainer(player.asInstanceOf[EntityPlayerMP], createContainer(player), new IGuiPacketSender
        {
            def sendPacket(player:EntityPlayerMP, windowId:Int)
            {
                val p = new PacketCustom(TransportationSPH.channel, TransportationSPH.gui_FirewallPipe_open)
                p.writeCoord(x, y, z)
                writeInfo(p)
                p.writeByte(windowId)
                p.sendToPlayer(player)
            }
        })
    }

    def createContainer(player:EntityPlayer) =
    {
        val cont = new SpecialContainer(player.inventory)
        import scala.collection.JavaConversions._
        var s = 0
        for (p <- BasicGuiUtils.createSlotArray(8, 8, 7, 5, 0, 0))
        {
            cont.addCustomSlot(new SlotExtended(filt, s, p.get1, p.get2).setGhosting(true))
            s += 1
        }
        cont.addPlayerInventory(8, 120).setShiftClick(false)
    }

    override def routeFilter(forSide:Int) =
        new PathFilter(filtExclude, itemset, allowRoute, allowBroadcast, allowCrafting, allowController, 0x7)

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
