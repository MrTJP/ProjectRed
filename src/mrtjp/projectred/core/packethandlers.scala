package mrtjp.projectred.core

import codechicken.lib.packet.{PacketCustom, ICustomPacketTile}
import codechicken.lib.vec.BlockCoord
import mrtjp.projectred.ProjectRedCore
import net.minecraft.client.Minecraft
import net.minecraft.world.World
import codechicken.lib.packet.PacketCustom.{IServerPacketHandler, IClientPacketHandler}
import net.minecraft.entity.player.EntityPlayerMP
import net.minecraft.network.play.{INetHandlerPlayServer, INetHandlerPlayClient}
import mrtjp.projectred.core.libmc.PRLib

class CorePH
{
    var channel = ProjectRedCore

    val tilePacket = 1
    val messagePacket = 2
    val guiPacket = 3

    def handleTilePacket(world:World, packet:PacketCustom, pos:BlockCoord)
    {
        val t = PRLib.getTileEntity(world, pos, classOf[ICustomPacketTile])
        if (t != null) t.handleDescriptionPacket(packet)
    }
}

object CoreCPH extends CorePH with IClientPacketHandler
{
    def handlePacket(packet:PacketCustom, mc:Minecraft, nethandler:INetHandlerPlayClient)
    {
        val world = mc.theWorld
        packet.getType match
        {
            case this.tilePacket => handleTilePacket(world, packet, packet.readCoord)
            case this.messagePacket => Messenger.addMessage(packet.readDouble, packet.readDouble, packet.readDouble, packet.readString)
            case this.guiPacket => GuiManager.receiveGuiPacket(packet)
        }
    }
}

object CoreSPH extends CorePH with IServerPacketHandler
{
    override def handlePacket(packet:PacketCustom, sender:EntityPlayerMP, nethandler:INetHandlerPlayServer)
    {
        packet.getType match
        {
            case this.tilePacket => handleTilePacket(sender.theItemInWorldManager.theWorld, packet, packet.readCoord())
        }
    }
}