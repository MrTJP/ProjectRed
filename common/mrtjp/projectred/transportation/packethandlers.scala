package mrtjp.projectred.transportation

import codechicken.core.ClientUtils
import codechicken.lib.packet.PacketCustom
import codechicken.lib.packet.PacketCustom.{IServerPacketHandler, IClientPacketHandler}
import codechicken.multipart.TMultiPart
import java.util
import mrtjp.projectred.ProjectRedTransportation
import mrtjp.projectred.core.BasicUtils
import mrtjp.projectred.core.utils.{ItemKeyStack, ItemKey}
import mrtjp.projectred.transportation.ItemRoutingChip.EnumRoutingChip
import net.minecraft.client.Minecraft
import net.minecraft.client.multiplayer.NetClientHandler
import net.minecraft.entity.player.{EntityPlayerMP, EntityPlayer}
import net.minecraft.item.ItemStack
import net.minecraft.network.NetServerHandler
import net.minecraft.world.World

class TransportationPH
{
    val channel = ProjectRedTransportation
}

object TransportationCPH extends TransportationPH with IClientPacketHandler
{
    def handlePacket(packet:PacketCustom, net:NetClientHandler, mc:Minecraft) = packet.getType match
    {
        case NetConstants.gui_InterfacePipe_open => openRoutedInterfacePipeGui(packet, mc.thePlayer)
        case NetConstants.gui_CraftingPipe_open => openCraftingPipeGui(packet, mc.thePlayer)
        case NetConstants.gui_Chipset_open => openChipsetGui(packet, mc.thePlayer)
        case NetConstants.gui_Request_open => openRequestGui(packet, mc)
        case NetConstants.gui_Request_list => receiveRequestList(packet, mc)
        case NetConstants.particle_Spawn => RouteFX.handleClientPacket(packet, mc.theWorld)
        case NetConstants.gui_RouterUtil_open => openRouterUtilGui(packet, mc)
        case NetConstants.gui_ExtensionPipe_open => openExtensionPipeGui(packet, mc)
    }

    private def openExtensionPipeGui(packet:PacketCustom, mc:Minecraft)
    {
        val p:TMultiPart = BasicUtils.getMultiPart(mc.theWorld, packet.readCoord, 6)
        if (p.isInstanceOf[RoutedExtensionPipePart])
        {
            val pipe:RoutedExtensionPipePart = p.asInstanceOf[RoutedExtensionPipePart]
            ClientUtils.openSMPGui(packet.readUByte, new GuiExtensionPipe(pipe.createContainer(mc.thePlayer), packet.readString))
        }
    }

    private def openRouterUtilGui(packet:PacketCustom, mc:Minecraft)
    {
        ClientUtils.openSMPGui(packet.readByte, new GuiChipUpgrade(new ItemRouterUtility.ChipUpgradeContainer(mc.thePlayer)))
    }

    private def receiveRequestList(packet:PacketCustom, mc:Minecraft)
    {
        if (mc.currentScreen.isInstanceOf[GuiRequester])
        {
            val gui = mc.currentScreen.asInstanceOf[GuiRequester]
            val size = packet.readInt
            val map = new util.HashMap[ItemKey, Integer](size)

            for (i <- 0 until size)
            {
                val stack = packet.readItemStack(true)
                map.put(ItemKey.get(stack), stack.stackSize)
            }

            gui.receiveContentList(map)
        }
    }

    private def openRequestGui(packet:PacketCustom, mc:Minecraft)
    {
        val p = BasicUtils.getMultiPart(mc.thePlayer.worldObj, packet.readCoord, 6)
        if (p.isInstanceOf[IWorldRequester]) mc.displayGuiScreen(new GuiRequester(p.asInstanceOf[IWorldRequester]))
    }

    private def openRoutedInterfacePipeGui(packet:PacketCustom, player:EntityPlayer)
    {
        val p = BasicUtils.getMultiPart(player.worldObj, packet.readCoord, 6)
        if (p.isInstanceOf[RoutedInterfacePipePart])
        {
            val pipe = p.asInstanceOf[RoutedInterfacePipePart]
            ClientUtils.openSMPGui(packet.readUByte, new GuiInterfacePipe(pipe.createContainer(player), pipe))
        }
    }

    private def openCraftingPipeGui(packet:PacketCustom, player:EntityPlayer)
    {
        val p = BasicUtils.getMultiPart(player.worldObj, packet.readCoord, 6)
        if (p.isInstanceOf[RoutedCraftingPipePart])
        {
            val pipe = p.asInstanceOf[RoutedCraftingPipePart]
            ClientUtils.openSMPGui(packet.readUByte, new GuiCraftingPipe(pipe.createContainer(player), pipe))
            pipe.priority = packet.readInt
        }
    }

    private def openChipsetGui(packet:PacketCustom, player:EntityPlayer)
    {
        val slot = packet.readByte
        player.inventory.currentItem = slot
        val stack = player.inventory.getStackInSlot(slot)
        if (stack != null)
        {
            val e = EnumRoutingChip.get(stack.getItemDamage)
            if (e != null)
            {
                val r = e.createChipset
                ClientUtils.openSMPGui(packet.readByte, RoutingChipGuiFactory(r.createContainer(player)))
            }
        }
    }
}

object TransportationSPH extends TransportationPH with IServerPacketHandler
{
    def handlePacket(packet:PacketCustom, nethandler:NetServerHandler, sender:EntityPlayerMP) = packet.getType match
    {
        case NetConstants.gui_ChipNBTSet => setChipNBT(packet, sender)
        case NetConstants.gui_CraftingPipe_action => handleCraftingPipeAction(packet, sender.worldObj)
        case NetConstants.gui_Request_action => handleRequestAction(packet, sender)
        case NetConstants.gui_Request_submit => handleRequestSubmit(packet, sender)
        case NetConstants.gui_Request_listRefresh => handleRequestListRefresh(packet, sender)
        case NetConstants.gui_RouterUtil_action => handleRouterUtilAction(packet, sender)
    }

    private def handleRouterUtilAction(packet:PacketCustom, sender:EntityPlayerMP)
    {
        val c = sender.openContainer
        if (c.isInstanceOf[ItemRouterUtility.ChipUpgradeContainer])
        {
            val r = c.asInstanceOf[ItemRouterUtility.ChipUpgradeContainer]
            val action = packet.readString
            if (action == "inst") r.installPossibleUpgrades()
        }
    }

    private def handleRequestListRefresh(packet:PacketCustom, sender:EntityPlayerMP)
    {
        val bc = packet.readCoord
        val t = BasicUtils.getMultiPart(sender.worldObj, bc, 6)
        if (t.isInstanceOf[IWorldRequester])
            sendRequestList(t.asInstanceOf[IWorldRequester], sender, packet.readBoolean, packet.readBoolean)
    }

    private def handleRequestAction(packet:PacketCustom, sender:EntityPlayerMP)
    {
        val bc = packet.readCoord
        val t = BasicUtils.getMultiPart(sender.worldObj, bc, 6)
        if (t.isInstanceOf[IWorldRequester])
        {
            val ident = packet.readString
            //do things
        }
    }

    private def sendRequestList(requester:IWorldRequester, player:EntityPlayerMP, collectBroadcast:Boolean, collectCrafts:Boolean)
    {
        val cpf = new CollectionPathFinder().setRequester(requester)
        cpf.setCollectBroadcasts(collectBroadcast).setCollectCrafts(collectCrafts)

        val map:util.Map[ItemKey, Integer] = cpf.collect.getCollection
        val packet2 = new PacketCustom(channel, NetConstants.gui_Request_list)
        packet2.writeInt(map.size)

        import scala.collection.JavaConversions._
        for (entry <- map.entrySet) packet2.writeItemStack(entry.getKey.makeStack(if (entry.getValue == null) 0 else entry.getValue), true)

        packet2.compressed.sendToPlayer(player)
    }

    private def handleRequestSubmit(packet:PacketCustom, sender:EntityPlayerMP)
    {
        val bc = packet.readCoord
        val t = BasicUtils.getMultiPart(sender.worldObj, bc, 6)
        if (t.isInstanceOf[IWorldRequester])
        {
            val r = new RequestConsole().setDestination(t.asInstanceOf[IWorldRequester])
            val pull = packet.readBoolean
            val craft = packet.readBoolean
            val partial = packet.readBoolean
            r.setCrafting(craft).setPulling(pull).setPartials(partial)

            val s = ItemKeyStack.get(packet.readItemStack(true))
            r.makeRequest(s)
            if (r.requested > 0)
            {
                sender.addChatMessage("Successfully requested "+r.requested+" of "+s.key.getName+".")
                RouteFX.spawnType1(RouteFX.color_request, 8, bc, sender.worldObj)
            }
            else
            {
                sender.addChatMessage("Could not request "+s.stackSize+" of "+s.key.getName+". Missing:")
                import scala.collection.JavaConversions._
                for (entry <- r.getMissing.entrySet) sender.addChatMessage(entry.getValue+" of "+entry.getKey.getName)
            }
            sendRequestList(t.asInstanceOf[IWorldRequester], sender, pull, craft)
        }
    }

    private def setChipNBT(packet:PacketCustom, player:EntityPlayerMP)
    {
        val slot:Int = packet.readByte
        val stack:ItemStack = packet.readItemStack
        player.inventory.setInventorySlotContents(slot, stack)
        player.inventory.onInventoryChanged()
    }

    private def handleCraftingPipeAction(packet:PacketCustom, w:World)
    {
        val t = BasicUtils.getMultiPart(w, packet.readCoord, 6)
        if (t.isInstanceOf[RoutedCraftingPipePart])
        {
            val pipe = t.asInstanceOf[RoutedCraftingPipePart]
            val action = packet.readString
            if (action == "up") pipe.priorityUp()
            else if (action == "down") pipe.priorityDown()
        }
    }
}
