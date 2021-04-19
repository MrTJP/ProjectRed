/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.integration

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.packet.ICustomPacketHandler.{IClientPacketHandler, IServerPacketHandler}
import codechicken.lib.packet.{PacketCustom, PacketCustomChannelBuilder}
import codechicken.lib.util.CrashLock
import codechicken.multipart.api.part.TMultiPart
import codechicken.multipart.block.BlockMultiPart
import mrtjp.projectred.ProjectRedIntegration
import mrtjp.projectred.integration.IntegrationNetwork._
import net.minecraft.client.Minecraft
import net.minecraft.client.network.play.IClientPlayNetHandler
import net.minecraft.entity.player.ServerPlayerEntity
import net.minecraft.network.play.IServerPlayNetHandler
import net.minecraft.util.ResourceLocation
import net.minecraft.world.World

//class IntegrationPH
//{
//    val channel = "PR|Integr"
//
//    def writePartIndex(out:MCDataOutput, part:TMultiPart) =
//    {
//        out.writePos(part.pos).writeByte(part.tile.partList.indexOf(part))
//    }
//
//    def readPartIndex(world:World, in:MCDataInput) =
//    {
//        val tile = BlockMultipart.getTile(world, in.readPos())//PRLib.getMultipartTile(world, in.readCoord)
//        try {
//            tile.partList(in.readUByte)
//        }
//        catch {
//            case e:NullPointerException => null
//            case e:IndexOutOfBoundsException => null
//        }
//    }
//}
//
//object IntegrationSPH extends IntegrationPH with IServerPacketHandler
//{
//    override def handlePacket(packet:PacketCustom, sender:EntityPlayerMP, handler:INetHandlerPlayServer) =
//        packet.getType match {
//            case 1 => incrTimer(sender.world, packet)
//            case 2 => incCounter(sender.world, packet)
//        }
//
//    private def incCounter(world:World, packet:PacketCustom)
//    {
//        readPartIndex(world, packet) match {
//            case gate:GatePart if gate.getLogic.isInstanceOf[ICounterGuiLogic] =>
//                val t = gate.getLogic[ICounterGuiLogic]
//                val actionID = packet.readByte()
//                if (actionID == 0) t.setCounterMax(gate, t.getCounterMax+packet.readShort())
//                else if (actionID == 1) t.setCounterIncr(gate, t.getCounterIncr+packet.readShort())
//                else if (actionID == 2) t.setCounterDecr(gate, t.getCounterDecr+packet.readShort())
//            case _ =>
//        }
//    }
//
//    private def incrTimer(world:World, packet:PacketCustom)
//    {
//        readPartIndex(world, packet) match {
//            case gate:GatePart if gate.getLogic.isInstanceOf[ITimerGuiLogic] =>
//                val t = gate.getLogic[ITimerGuiLogic]
//                t.setTimerMax(gate, t.getTimerMax+packet.readShort())
//            case _ =>
//        }
//    }
//
//}
//
//object IntegrationCPH extends IntegrationPH with IClientPacketHandler
//{
//    override def handlePacket(packet:PacketCustom, mc:Minecraft, handler:INetHandlerPlayClient) =
//        packet.getType match {
//            case _ =>
//        }
//}


object IntegrationNetwork
{
    private val LOCK = new CrashLock("Already initialized.")
    val NET_CHANNEL = new ResourceLocation(ProjectRedIntegration.MOD_ID, "network")



//    val C_TILE_UPDATE = 1
//    val C_ADD_MESSAGE = 2
//    val C_GUI_PACKET = 3
//
//    val S_TILE_UPDATE = 1
//    val S_KEY_UPDATE = 2

    // Server to client messages
    val OPEN_TIMER_GUI_FROM_SERVER = 1
    val OPEN_COUNTER_GUI_FROM_SERVER = 2

    // Client to server messages
    val INCR_TIMER_FROM_CLIENT = 3
    val INCR_COUNTER_FROM_CLIENT = 4

    def init() {
        LOCK.lock()
        PacketCustomChannelBuilder.named(NET_CHANNEL)
                .assignClientHandler(() => () => ClientHandler)
                .assignServerHandler(() => () => ServerHandler)
                .build()
    }

    def writePartIndex(out:MCDataOutput, part:TMultiPart) = {
        out.writePos(part.pos).writeByte(part.tile.getPartList.indexOf(part)) //TODO use slotted part index
    }

    def readPartIndex(world:World, in:MCDataInput) = {
        val tile = BlockMultiPart.getTile(world, in.readPos())//PRLib.getMultipartTile(world, in.readCoord)
        try {
            tile.getPartList.get(in.readUByte)
        }
        catch {
            case e:NullPointerException => null
            case e:IndexOutOfBoundsException => null
        }
    }

//    def openTimerGui(player:PlayerEntity, gate:GatePart):Unit = {
//        val packet = new PacketCustom(NET_CHANNEL, OPEN_TIMER_GUI_FROM_SERVER)
//        writePartIndex(packet, gate)
//        packet.sendToPlayer(player.asInstanceOf[ServerPlayerEntity])
//    }
//
//    def openCounterGui(player:PlayerEntity, gate:GatePart):Unit = {
//        val packet = new PacketCustom(NET_CHANNEL, OPEN_COUNTER_GUI_FROM_SERVER)
//        writePartIndex(packet, gate)
//        packet.sendToPlayer(player.asInstanceOf[ServerPlayerEntity])
//    }
}

private object ClientHandler extends IClientPacketHandler
{
    override def handlePacket(packet: PacketCustom, mc: Minecraft, handler: IClientPlayNetHandler):Unit = {
        packet.getType match {
            case OPEN_TIMER_GUI_FROM_SERVER => handleOpenTimerGuiMessage(mc, packet)
            case OPEN_COUNTER_GUI_FROM_SERVER => handleOpenCounterGuiMessage(mc, packet)
            case _ =>
        }
    }

    private def handleOpenTimerGuiMessage(mc:Minecraft, data:MCDataInput) {
        readPartIndex(mc.world, data) match  {
            case gate:ITimerGuiLogic =>
                mc.displayGuiScreen(new GuiTimer(gate))
            case _ =>
        }
    }

    private def handleOpenCounterGuiMessage(mc:Minecraft, data:MCDataInput) {
        readPartIndex(mc.world, data) match  {
            case gate:ICounterGuiLogic =>
                mc.displayGuiScreen(new GuiCounter(gate))
            case _ =>
        }
    }
}

private object ServerHandler extends IServerPacketHandler
{
    override def handlePacket(packet: PacketCustom, sender: ServerPlayerEntity, handler: IServerPlayNetHandler):Unit = {
        packet.getType match  {
            case INCR_TIMER_FROM_CLIENT => incrTimer(sender.world, packet)
            case INCR_COUNTER_FROM_CLIENT => incrCounter(sender.world, packet)
        }
    }

    private def incrCounter(world:World, packet:PacketCustom):Unit = {
        readPartIndex(world, packet) match {
            case gate:ICounterGuiLogic =>
                val actionID = packet.readByte()
                if (actionID == 0) gate.setCounterMax(gate.getCounterMax+packet.readShort())
                else if (actionID == 1) gate.setCounterIncr(gate.getCounterIncr+packet.readShort())
                else if (actionID == 2) gate.setCounterDecr(gate.getCounterDecr+packet.readShort())
            case _ =>
        }
    }

    private def incrTimer(world:World, packet:PacketCustom):Unit = {
        readPartIndex(world, packet) match {
            case gate:ITimerGuiLogic =>
                gate.setTimerMax(gate.getTimerMax+packet.readShort())
            case _ =>
        }
    }
}