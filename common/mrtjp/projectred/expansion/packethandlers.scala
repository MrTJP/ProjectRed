package mrtjp.projectred

import codechicken.lib.packet.PacketCustom.{IServerPacketHandler, IClientPacketHandler}
import codechicken.lib.packet.PacketCustom
import net.minecraft.client.multiplayer.NetClientHandler
import net.minecraft.client.Minecraft
import net.minecraft.network.NetServerHandler
import net.minecraft.entity.player.{EntityPlayer, EntityPlayerMP}
import mrtjp.projectred.core.BasicUtils
import mrtjp.projectred.expansion.{MachineGuiFactory, TileMachineIO}
import codechicken.core.ClientUtils

class ExpansionPH
{
    val channel = ProjectRedExpansion
    val machine_gui_open = 1
}

object ExpansionCPH extends ExpansionPH with IClientPacketHandler
{
    def handlePacket(packet:PacketCustom, nethandler:NetClientHandler, mc:Minecraft)
    {
        packet.getType match {
            case machine_gui_open => openMachineGui(packet, mc)
        }
    }

    def openMachineGui(packet:PacketCustom, mc:Minecraft)
    {
        val machine = BasicUtils.getTileEntity(mc.theWorld, packet.readCoord(), classOf[TileMachineIO])
        if (machine != null)
            ClientUtils.openSMPGui(packet.readUByte(), MachineGuiFactory.createGui(packet.readUByte(), machine))
    }
}

object ExpansionSPH extends ExpansionPH with IServerPacketHandler
{
    def handlePacket(packet:PacketCustom, nethandler:NetServerHandler, sender:EntityPlayerMP)
    {
    }
}