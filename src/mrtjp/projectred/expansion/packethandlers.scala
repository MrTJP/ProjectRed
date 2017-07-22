package mrtjp.projectred.expansion

import codechicken.lib.packet.PacketCustom
import codechicken.lib.packet.ICustomPacketHandler.{IClientPacketHandler, IServerPacketHandler}
import mrtjp.projectred.ProjectRedExpansion
import net.minecraft.client.Minecraft
import net.minecraft.entity.player.EntityPlayerMP
import net.minecraft.network.play.{INetHandlerPlayClient, INetHandlerPlayServer}

class ExpansionPH
{
    val channel = ProjectRedExpansion
    val jetpack_state = 1
}

object ExpansionCPH extends ExpansionPH with IClientPacketHandler
{
    def handlePacket(packet:PacketCustom, mc:Minecraft, nethandler:INetHandlerPlayClient)
    {
        packet.getType match
        {
            case `jetpack_state` =>
                ItemJetpack.setStateOfEntity(packet.readInt(), packet.readBoolean(), false)
        }
    }

    def openMachineGui(packet:PacketCustom, mc:Minecraft)
    {
    }
}

object ExpansionSPH extends ExpansionPH with IServerPacketHandler
{
    def handlePacket(packet:PacketCustom, sender:EntityPlayerMP, nethandler:INetHandlerPlayServer)
    {
    }
}
