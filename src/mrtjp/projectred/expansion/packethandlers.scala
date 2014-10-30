package mrtjp.projectred.expansion

import codechicken.lib.packet.PacketCustom
import codechicken.lib.packet.PacketCustom.{IClientPacketHandler, IServerPacketHandler}
import mrtjp.projectred.ProjectRedExpansion
import net.minecraft.client.Minecraft
import net.minecraft.entity.player.EntityPlayerMP
import net.minecraft.network.play.{INetHandlerPlayClient, INetHandlerPlayServer}

class ExpansionPH
{
    val channel = ProjectRedExpansion
    val machine_gui_open = 1
}

object ExpansionCPH extends ExpansionPH with IClientPacketHandler
{
    def handlePacket(packet:PacketCustom, mc:Minecraft, nethandler:INetHandlerPlayClient)
    {
        packet.getType match
        {
            case this.machine_gui_open => openMachineGui(packet, mc)
        }
    }

    def openMachineGui(packet:PacketCustom, mc:Minecraft)
    {
//        val machine = WorldLib.getTileEntity(mc.theWorld, packet.readCoord(), classOf[TileGuiMachine])
//        if (machine != null)
//            ClientUtils.openSMPGui(packet.readUByte(), MachineGuiFactory(packet.readUByte(), machine))
    }
}

object ExpansionSPH extends ExpansionPH with IServerPacketHandler
{
    def handlePacket(packet:PacketCustom, sender:EntityPlayerMP, nethandler:INetHandlerPlayServer)
    {
    }
}