/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.handler

import codechicken.lib.packet.ICustomPacketHandler.{IClientPacketHandler, IServerPacketHandler}
import codechicken.lib.packet.{PacketCustom, PacketCustomChannelBuilder}
import codechicken.lib.util.CrashLock
import mrtjp.core.block.TPacketTile
import mrtjp.core.data.KeyTracking
import mrtjp.core.handler.MrTJPCoreMod.MOD_ID
import mrtjp.core.handler.MrTJPCoreNetwork._
import mrtjp.core.world.Messenger
import net.minecraft.client.Minecraft
import net.minecraft.client.network.play.IClientPlayNetHandler
import net.minecraft.entity.player.ServerPlayerEntity
import net.minecraft.network.play.IServerPlayNetHandler
import net.minecraft.util.ResourceLocation
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World

object MrTJPCoreNetwork
{
    private val LOCK = new CrashLock("Already initialized.")
    val NET_CHANNEL = new ResourceLocation(MOD_ID, "network")

    val C_TILE_UPDATE = 1
    val C_ADD_MESSAGE = 2
//    val guiPacket = 3

    val S_TILE_UPDATE = 1
    val S_KEY_UPDATE = 2

    def init() {
        LOCK.lock()
        PacketCustomChannelBuilder.named(NET_CHANNEL)
            .assignClientHandler(() => () => ClientHandler)
            .assignServerHandler(() => () => ServerHandler)
            .build()
    }

    private[handler] def handleTilePacket(world:World, packet:PacketCustom, pos:BlockPos)
    {
        world.getTileEntity(pos) match {
            case cpt:TPacketTile => cpt.readFromPacket(packet)
            case _ =>
        }
    }

    def sendTilePacket(world:World, pos:BlockPos, packet:PacketCustom)
    {

    }
}

private object ClientHandler extends IClientPacketHandler
{
    override def handlePacket(packet: PacketCustom, mc: Minecraft, handler: IClientPlayNetHandler)
    {
        val world = mc.world
        packet.getType match {
            case C_TILE_UPDATE => handleTilePacket(world, packet, packet.readPos())
            case C_ADD_MESSAGE => Messenger.addMessage(packet.readDouble, packet.readDouble, packet.readDouble, packet.readString)
//            case this.guiPacket => GuiHandler.receiveGuiPacket(packet)
        }
    }
}

private object ServerHandler extends IServerPacketHandler
{
    override def handlePacket(packet: PacketCustom, sender: ServerPlayerEntity, handler: IServerPlayNetHandler)
    {
        packet.getType match
        {
            case S_TILE_UPDATE => handleTilePacket(sender.getEntityWorld, packet, packet.readPos())
            case S_KEY_UPDATE => KeyTracking.updatePlayerKey(packet.readUByte(), sender, packet.readBoolean())
        }
    }
}
