package mrtjp.projectred.transportation

import codechicken.lib.packet.PacketCustom
import codechicken.lib.packet.PacketCustom.{IClientPacketHandler, IServerPacketHandler}
import mrtjp.core.item.{ItemKey, ItemKeyStack}
import mrtjp.projectred.core.libmc.PRLib
import net.minecraft.client.Minecraft
import net.minecraft.entity.player.EntityPlayerMP
import net.minecraft.network.play.{INetHandlerPlayClient, INetHandlerPlayServer}
import net.minecraft.util.ChatComponentText

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

    val gui_RouterUtil_action = 13

    val gui_FirewallPipe_action = 17
}

object TransportationCPH extends TransportationPH with IClientPacketHandler
{
    def handlePacket(packet:PacketCustom, mc:Minecraft, handler:INetHandlerPlayClient) = packet.getType match
    {
        case this.gui_Request_open => openRequestGui(packet, mc)
        case this.gui_Request_list => receiveRequestList(packet, mc)
        case this.particle_Spawn => RouteFX2.handleClientPacket(packet, mc.theWorld)
        case _ =>
    }

    private def receiveRequestList(packet:PacketCustom, mc:Minecraft)
    {
        if (mc.currentScreen.isInstanceOf[GuiRequester])
        {
            val gui = mc.currentScreen.asInstanceOf[GuiRequester]
            val size = packet.readInt
            var map2 = Map[ItemKey, Int]()

            for (i <- 0 until size)
            {
                val stack = packet.readItemStack(true)
                map2 += ItemKey.get(stack) -> stack.stackSize
            }

            gui.receiveContentList(map2)
        }
    }

    private def openRequestGui(packet:PacketCustom, mc:Minecraft)
    {
        val p = PRLib.getMultiPart(mc.thePlayer.worldObj, packet.readCoord, 6)
        if (p.isInstanceOf[IWorldRequester]) mc.displayGuiScreen(new GuiRequester(p.asInstanceOf[IWorldRequester]))
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
        case this.gui_RouterUtil_action => handleRouterUtilAction(packet, sender)
        case this.gui_FirewallPipe_action => handleFirewallAction(packet, sender)
        case _ =>
    }

    private def handleFirewallAction(packet:PacketCustom, sender:EntityPlayerMP)
    {
        val bc = packet.readCoord()
        val action = packet.readByte()
        val t = PRLib.getMultiPart(sender.worldObj, bc, 6)
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

    private def handleRouterUtilAction(packet:PacketCustom, sender:EntityPlayerMP)
    {
        val c = sender.openContainer
        if (c.isInstanceOf[ChipUpgradeContainer])
        {
            val r = c.asInstanceOf[ChipUpgradeContainer]
            val action = packet.readString
            if (action == "inst") r.install()
        }
    }

    private def handleRequestListRefresh(packet:PacketCustom, sender:EntityPlayerMP)
    {
        val bc = packet.readCoord
        val t = PRLib.getMultiPart(sender.worldObj, bc, 6)
        if (t.isInstanceOf[IWorldRequester])
            sendRequestList(t.asInstanceOf[IWorldRequester], sender, packet.readBoolean, packet.readBoolean)
    }

    private def handleRequestAction(packet:PacketCustom, sender:EntityPlayerMP)
    {
        val bc = packet.readCoord
        val t = PRLib.getMultiPart(sender.worldObj, bc, 6)
        if (t.isInstanceOf[IWorldRequester])
        {
            val ident = packet.readString
            //do things
        }
    }

    private def sendRequestList(requester:IWorldRequester, player:EntityPlayerMP, collectBroadcast:Boolean, collectCrafts:Boolean)
    {
        CollectionPathFinder.clear()
        CollectionPathFinder.start = requester
        CollectionPathFinder.collectBroadcasts = collectBroadcast
        CollectionPathFinder.collectCrafts = collectCrafts
        val map = CollectionPathFinder.result()
        CollectionPathFinder.clear()

        val packet2 = new PacketCustom(channel, gui_Request_list)
        packet2.writeInt(map.size)

        for ((k,v) <- map) packet2.writeItemStack(k.makeStack(v), true)

        packet2.compress().sendToPlayer(player)
    }

    private def handleRequestSubmit(packet:PacketCustom, sender:EntityPlayerMP)
    {
        val bc = packet.readCoord
        val t = PRLib.getMultiPart(sender.worldObj, bc, 6)
        if (t.isInstanceOf[IWorldRequester])
        {
            import mrtjp.projectred.transportation.RequestFlags._
            var opt = RequestFlags.ValueSet.newBuilder
            val pull = packet.readBoolean
            val craft = packet.readBoolean
            val partial = packet.readBoolean
            if (pull) opt += PULL
            if (craft) opt += CRAFT
            if (partial) opt += PARTIAL

            val r = new RequestConsole(opt.result()).setDestination(t.asInstanceOf[IWorldRequester])
            val s = ItemKeyStack.get(packet.readItemStack(true))

            r.buildRequestTree(s)

            r.startRequest()

            if (r.requested > 0)
            {
                sender.addChatMessage(new ChatComponentText("Successfully requested "+r.requested+" of "+s.key.getName+"."))
                RouteFX2.spawnType1(RouteFX2.color_request, t.asInstanceOf[IWorldRouter].getContainer)
            }
            else
            {
                sender.addChatMessage(new ChatComponentText("Could not request "+s.stackSize+" of "+s.key.getName+". Missing:"))
                for ((k,v) <- r.getMissing) sender.addChatMessage(new ChatComponentText(v+" of "+k.getName))
            }

            sendRequestList(t.asInstanceOf[IWorldRequester], sender, pull, craft)
        }
    }

    private def setChipNBT(packet:PacketCustom, player:EntityPlayerMP)
    {
        val slot = packet.readUByte()
        val stack = packet.readItemStack()
        player.inventory.setInventorySlotContents(slot, stack)
        player.inventory.markDirty()
    }
}
