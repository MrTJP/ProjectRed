package mrtjp.projectred.integration

import codechicken.lib.packet.PacketCustom
import codechicken.lib.packet.PacketCustom.{IServerPacketHandler, IClientPacketHandler}
import codechicken.lib.vec.BlockCoord
import codechicken.multipart.TMultiPart
import mrtjp.projectred.ProjectRedIntegration
import net.minecraft.client.Minecraft
import net.minecraft.client.multiplayer.NetClientHandler
import net.minecraft.entity.player.{EntityPlayer, EntityPlayerMP}
import net.minecraft.network.NetServerHandler
import net.minecraft.world.World
import mrtjp.projectred.core.libmc.BasicUtils

class IntegrationPH
{
    val channel = ProjectRedIntegration

    def writePartIndex(out:PacketCustom, part:TMultiPart) =
    {
        out.writeCoord(new BlockCoord(part.tile)).writeByte(part.tile.jPartList.indexOf(part))
    }

    def readPartIndex(world:World, in:PacketCustom):TMultiPart =
    {
        val tile = BasicUtils.getMultipartTile(world, in.readCoord)
        try
        {
            tile.jPartList.get(in.readUByte)
        }
        catch
        {
            case e:NullPointerException => return null
            case e:IndexOutOfBoundsException => return null
        }
    }

    def openTimerGui(player:EntityPlayer, part:GatePart)
    {
        val packet = new PacketCustom(channel, 1)
        writePartIndex(packet, part)
        packet.sendToPlayer(player)
    }

    def openCounterGui(player:EntityPlayer, part:GatePart)
    {
        val packet = new PacketCustom(channel, 2)
        writePartIndex(packet, part)
        packet.sendToPlayer(player)
    }
}

object IntegrationSPH extends IntegrationPH with IServerPacketHandler
{
    override def handlePacket(packet:PacketCustom, nethandler:NetServerHandler, sender:EntityPlayerMP) = packet.getType match
    {
        case 1 => incrTimer(sender.worldObj, packet)
        case 2 => incCounter(sender.worldObj, packet)
    }

    private def incCounter(world:World, packet:PacketCustom)
    {
        val part = readPartIndex(world, packet)
        if (part.isInstanceOf[GatePart])
        {
            val gate = part.asInstanceOf[GatePart]
            if (gate.getLogic.isInstanceOf[GateLogic.ICounterGuiLogic])
            {
                val t = gate.getLogic.asInstanceOf[GateLogic.ICounterGuiLogic]
                val actionID:Int = packet.readByte
                if (actionID == 0) t.setCounterMax(gate, t.getCounterMax + packet.readShort)
                else if (actionID == 1) t.setCounterIncr(gate, t.getCounterIncr + packet.readShort)
                else if (actionID == 2) t.setCounterDecr(gate, t.getCounterDecr + packet.readShort)
            }
        }
    }

    private def incrTimer(world:World, packet:PacketCustom)
    {
        val part = readPartIndex(world, packet)
        if (part.isInstanceOf[GatePart])
        {
            val gate = part.asInstanceOf[GatePart]
            if (gate.getLogic.isInstanceOf[GateLogic.ITimerGuiLogic])
            {
                val t = gate.getLogic.asInstanceOf[GateLogic.ITimerGuiLogic]
                t.setTimerMax(gate, t.getTimerMax + packet.readShort)
            }
        }
    }
}

object IntegrationCPH extends IntegrationPH with IClientPacketHandler
{
    override def handlePacket(packet:PacketCustom, nethandler:NetClientHandler, mc:Minecraft) = packet.getType match
    {
        case 1 => openTimerGui(mc, mc.theWorld, packet)
        case 2 => openCounterGui(mc, mc.theWorld, packet)
    }

    private def openTimerGui(mc:Minecraft, world:World, packet:PacketCustom)
    {
        val part = readPartIndex(world, packet)
        if (part.isInstanceOf[GatePart])
        {
            val gate = part.asInstanceOf[GatePart]
            if (gate.getLogic.isInstanceOf[GateLogic.ITimerGuiLogic]) mc.displayGuiScreen(new GuiTimer(gate))
        }
    }

    private def openCounterGui(mc:Minecraft, world:World, packet:PacketCustom)
    {
        val part = readPartIndex(world, packet)
        if (part.isInstanceOf[GatePart])
        {
            val gate = part.asInstanceOf[GatePart]
            if (gate.getLogic.isInstanceOf[GateLogic.ICounterGuiLogic]) mc.displayGuiScreen(new GuiCounter(gate))
        }
    }
}
