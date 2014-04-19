package mrtjp.projectred.core

import codechicken.lib.packet.{PacketCustom, ICustomPacketTile}
import codechicken.lib.vec.BlockCoord
import mrtjp.projectred.ProjectRedCore
import net.minecraft.client.Minecraft
import net.minecraft.world.World
import codechicken.lib.packet.PacketCustom.{IServerPacketHandler, IClientPacketHandler}
import net.minecraft.entity.player.EntityPlayerMP
import net.minecraft.network.play.{INetHandlerPlayServer, INetHandlerPlayClient}
import mrtjp.projectred.core.libmc.BasicUtils

class CorePH
{
    var channel = ProjectRedCore

    var tilePacket = 1
    var messagePacket = 2

    def handleTilePacket(world:World, packet:PacketCustom, pos:BlockCoord)
    {
        val t = BasicUtils.getTileEntity(world, pos, classOf[ICustomPacketTile])
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
            case 1 => handleTilePacket(world, packet, packet.readCoord)
            case 2 => Messenger.addMessage(packet.readDouble, packet.readDouble, packet.readDouble, packet.readString)
        }

    }
}

object CoreSPH extends CorePH with IServerPacketHandler
{
    override def handlePacket(packet:PacketCustom, sender:EntityPlayerMP, nethandler:INetHandlerPlayServer)
    {
        packet.getType match
        {
            case 1 => handleTilePacket(sender.theItemInWorldManager.theWorld, packet, packet.readCoord())
        }
    }
}