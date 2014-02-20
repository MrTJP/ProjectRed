package mrtjp.projectred.core

import codechicken.lib.packet.{PacketCustom, ICustomPacketTile}
import codechicken.lib.vec.BlockCoord
import mrtjp.projectred.ProjectRedCore
import net.minecraft.client.Minecraft
import net.minecraft.client.multiplayer.NetClientHandler
import net.minecraft.world.World
import codechicken.lib.packet.PacketCustom.{IServerPacketHandler, IClientPacketHandler}
import net.minecraft.network.NetServerHandler
import net.minecraft.entity.player.EntityPlayerMP

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
    override def handlePacket(packet:PacketCustom, nethandler:NetClientHandler, mc:Minecraft)
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
    override def handlePacket(packet:PacketCustom, nethandler:NetServerHandler, sender:EntityPlayerMP)
    {
        packet.getType match
        {
            case 1 => handleTilePacket(sender.theItemInWorldManager.theWorld, packet, packet.readCoord())
        }
    }
}