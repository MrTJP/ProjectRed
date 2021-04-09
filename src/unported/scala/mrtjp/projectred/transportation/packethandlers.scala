package mrtjp.projectred.transportation

import codechicken.lib.packet.PacketCustom
import codechicken.lib.packet.ICustomPacketHandler.{IClientPacketHandler, IServerPacketHandler}
import codechicken.multipart.BlockMultipart
import mrtjp.core.item.{ItemKey, ItemKeyStack}
import mrtjp.projectred.ProjectRedTransportation
import net.minecraft.client.Minecraft
import net.minecraft.entity.player.EntityPlayerMP
import net.minecraft.network.play.{INetHandlerPlayClient, INetHandlerPlayServer}
import net.minecraft.util.text.TextComponentString

class TransportationPH
{
    val channel = "PR|Transp"

    val gui_ChipNBTSet = 4

    val gui_Request_open = 6
    val gui_Request_action = 7
    val gui_Request_submit = 8
    val gui_Request_list = 9
    val gui_Request_listRefresh = 10

    val particle_Spawn = 11

    val gui_FirewallPipe_action = 17
}

object TransportationCPH extends TransportationPH with IClientPacketHandler
{
    def handlePacket(packet:PacketCustom, mc:Minecraft, handler:INetHandlerPlayClient) = packet.getType match
    {
        case this.gui_Request_open => openRequestGui(packet, mc)
        case this.gui_Request_list => receiveRequestList(packet, mc)
        case this.particle_Spawn => RouteFX2.handleClientPacket(packet, mc.world)
        case _ =>
    }

    private def receiveRequestList(packet:PacketCustom, mc:Minecraft)
    {
        if (mc.currentScreen.isInstanceOf[GuiRequester]) {
            val gui = mc.currentScreen.asInstanceOf[GuiRequester]
            val size = packet.readInt
            var map2 = Map[ItemKey, Int]()

            for (i <- 0 until size) {
                val stack = packet.readItemStack()
                map2 += ItemKey.get(stack) -> stack.getCount
            }

            gui.receiveContentList(map2)
        }
    }

    private def openRequestGui(packet:PacketCustom, mc:Minecraft)
    {
        val p = BlockMultipart.getPart(mc.player.world, packet.readPos(), 6)
        if (p.isInstanceOf[IRouterContainer]) mc.displayGuiScreen(new GuiRequester(p.asInstanceOf[IRouterContainer]))
    }
}

object TransportationSPH extends TransportationPH with IServerPacketHandler
{
    def handlePacket(packet:PacketCustom, sender:EntityPlayerMP, handler:INetHandlerPlayServer) = packet.getType match
    {
        case this.gui_ChipNBTSet => setChipNBT(packet, sender)
        case this.gui_Request_action => handleRequestAction(packet, sender)
        case this.gui_Request_submit => handleRequestSubmit(packet, sender)
        case this.gui_Request_listRefresh => handleRequestListRefresh(packet, sender)
        case this.gui_FirewallPipe_action => handleFirewallAction(packet, sender)
        case _ =>
    }

    private def handleFirewallAction(packet:PacketCustom, sender:EntityPlayerMP)
    {
        val bc = packet.readPos()
        val action = packet.readByte()
        val t = BlockMultipart.getPart(sender.world, bc, 6)
        if (t.isInstanceOf[RoutedFirewallPipe])
        {
            val p = t.asInstanceOf[RoutedFirewallPipe]
            action match
            {
                case 0 => p.filtExclude = !p.filtExclude
                case 1 => p.allowRoute = !p.allowRoute
                case 2 => p.allowBroadcast = !p.allowBroadcast
                case 3 => p.allowCrafting = !p.allowCrafting
            }
            p.sendOptUpdate()
        }
    }

//    private def handleRouterUtilAction(packet:PacketCustom, sender:EntityPlayerMP)
//    {
//        val c = sender.openContainer
//        if (c.isInstanceOf[ChipUpgradeContainer])
//        {
//            val r = c.asInstanceOf[ChipUpgradeContainer]
//            val action = packet.readString
//            if (action == "inst") r.install()
//        }
//    }

    private def handleRequestListRefresh(packet:PacketCustom, sender:EntityPlayerMP)
    {
        val t = BlockMultipart.getPart(sender.world, packet.readPos(), 6)
        if (t.isInstanceOf[IRouterContainer])
            sendRequestList(t.asInstanceOf[IRouterContainer], sender, packet.readBoolean, packet.readBoolean)
    }

    private def handleRequestAction(packet:PacketCustom, sender:EntityPlayerMP)
    {
        val t = BlockMultipart.getPart(sender.world, packet.readPos(), 6)
        if (t.isInstanceOf[IRouterContainer]) {
            val ident = packet.readString
            //do things
        }
    }

    private def sendRequestList(requester:IRouterContainer, player:EntityPlayerMP, collectBroadcast:Boolean, collectCrafts:Boolean)
    {
        CollectionPathFinder.clear()
        CollectionPathFinder.start = requester
        CollectionPathFinder.collectBroadcasts = collectBroadcast
        CollectionPathFinder.collectCrafts = collectCrafts
        val map = CollectionPathFinder.result()
        CollectionPathFinder.clear()

        val packet2 = new PacketCustom(channel, gui_Request_list)
        packet2.writeInt(map.size)

        for ((k,v) <- map) {
            val s = if (v == 0) 1 else v //TODO find way to mark craft items.
            packet2.writeItemStack(k.makeStack(s))
        }

        packet2.compress().sendToPlayer(player)
    }

    private def handleRequestSubmit(packet:PacketCustom, sender:EntityPlayerMP)
    {
        val t = BlockMultipart.getPart(sender.world, packet.readPos(), 6)
        if (t.isInstanceOf[IRouterContainer]) {
            import mrtjp.projectred.transportation.RequestFlags._
            var opt = RequestFlags.ValueSet.newBuilder
            val pull = packet.readBoolean
            val craft = packet.readBoolean
            val partial = packet.readBoolean
            if (pull) opt += PULL
            if (craft) opt += CRAFT
            if (partial) opt += PARTIAL

            val r = new RequestConsole(opt.result()).setDestination(t.asInstanceOf[IRouterContainer])
            val s = ItemKeyStack.get(packet.readItemStack())

            r.buildRequestTree(s)

            r.startRequest()

            if (r.requested > 0)
            {
                sender.sendMessage(new TextComponentString("Successfully requested "+r.requested+" of "+s.key.getName+"."))
                RouteFX2.spawnType1(RouteFX2.color_request, t.asInstanceOf[IRouterContainer].getPipe)
            }
            else
            {
                sender.sendMessage(new TextComponentString("Could not request "+s.stackSize+" of "+s.key.getName+". Missing:"))
                for ((k,v) <- r.getMissing) sender.sendMessage(new TextComponentString(v+" of "+k.getName))
            }

            sendRequestList(t.asInstanceOf[IRouterContainer], sender, pull, craft)
        }
    }

    private def setChipNBT(packet:PacketCustom, player:EntityPlayerMP)
    {
        val slot = packet.readUByte()
        val stack = packet.readItemStack()
        if (stack.getItem == ProjectRedTransportation.itemRoutingChip) {
            val playerStack = player.inventory.getStackInSlot(slot)
            if (playerStack.getItem == ProjectRedTransportation.itemRoutingChip) {

                val chip = ItemRoutingChip.loadChipFromItemStack(stack)
                ItemRoutingChip.saveChipToItemStack(playerStack, chip)

                player.inventory.setInventorySlotContents(slot, playerStack)
                player.inventory.markDirty()
            }
        }
    }
}
