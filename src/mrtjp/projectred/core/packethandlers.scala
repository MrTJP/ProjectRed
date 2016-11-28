package mrtjp.projectred.core

import codechicken.lib.packet.PacketCustom
import codechicken.lib.packet.PacketCustom.{IClientPacketHandler, IServerPacketHandler}
import mrtjp.projectred.ProjectRedCore
import net.minecraft.client.Minecraft
import net.minecraft.entity.player.EntityPlayerMP
import net.minecraft.network.play.{INetHandlerPlayClient, INetHandlerPlayServer}

class CorePH
{
    var channel = ProjectRedCore
}

object CoreCPH extends CorePH with IClientPacketHandler
{
    def handlePacket(packet:PacketCustom, mc:Minecraft, nethandler:INetHandlerPlayClient)
    {
        val world = mc.theWorld
        packet.getType match
        {
            case _ =>
        }
    }
}

object CoreSPH extends CorePH with IServerPacketHandler
{
    override def handlePacket(packet:PacketCustom, sender:EntityPlayerMP, nethandler:INetHandlerPlayServer)
    {
        packet.getType match
        {
            case _ =>
        }
    }
}