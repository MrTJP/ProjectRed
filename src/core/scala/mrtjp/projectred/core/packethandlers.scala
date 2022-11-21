package mrtjp.projectred.core

import codechicken.lib.data.MCDataInput
import codechicken.lib.packet.ICustomPacketHandler.{IClientPacketHandler, IServerPacketHandler}
import codechicken.lib.packet.{PacketCustom, PacketCustomChannelBuilder}
import codechicken.lib.util.CrashLock
import mrtjp.projectred.ProjectRedCore
import mrtjp.projectred.core.CoreNetwork._
import mrtjp.projectred.core.tile.{IBlockEventTile, IPacketReceiverTile}
import net.minecraft.client.Minecraft
import net.minecraft.client.network.play.IClientPlayNetHandler
import net.minecraft.entity.player.ServerPlayerEntity
import net.minecraft.network.play.IServerPlayNetHandler
import net.minecraft.util.ResourceLocation
import net.minecraft.world.World

object CoreNetwork
{
    private val LOCK = new CrashLock("Already initialized.")
    val NET_CHANNEL = new ResourceLocation(ProjectRedCore.MOD_ID, "network")

    // Server to client messages
    val TILE_UPDATE_FROM_SERVER = 1

    // Client to server messages
    val TILE_UPDATE_FROM_CLIENT = 1

    // Server to client messages
    val NET_TILE_PACKET_TO_CLIENT = 2

    // Client to server messages
    val NET_TILE_PACKET_TO_SERVER = 3

    def init() {
        LOCK.lock()
        PacketCustomChannelBuilder.named(NET_CHANNEL)
                .assignClientHandler(() => () => ClientHandler)
                .assignServerHandler(() => () => ServerHandler)
                .build()
    }


    def createUpdatePacket(tile:CoreTile):PacketCustom = {
        val packet = new PacketCustom(NET_CHANNEL,
            if (!tile.getLevel.isClientSide) TILE_UPDATE_FROM_SERVER else TILE_UPDATE_FROM_CLIENT)
        packet.writePos(tile.getBlockPos)
        packet
    }

    def createPacketReceiverTileClientPacket(tile:IPacketReceiverTile, key:Int):PacketCustom = {
        val packet = new PacketCustom(NET_CHANNEL, NET_TILE_PACKET_TO_CLIENT)
        packet.writePos(tile.getBlockPosition)
        packet.writeInt(key) //TODO make byte?
        packet
    }

    def createPacketReceiverTileServerPacket(tile:IPacketReceiverTile, key:Int):PacketCustom = {
        val packet = new PacketCustom(NET_CHANNEL, NET_TILE_PACKET_TO_SERVER)
        packet.writePos(tile.getBlockPosition)
        packet.writeInt(key) //TODO make byte?
        packet
    }

    private[core] def handleTileUpdate(world:World, input:MCDataInput):Unit = {
        val tile = world.getBlockEntity(input.readPos())
        if (tile != null && tile.isInstanceOf[CoreTile])
            tile.asInstanceOf[CoreTile].handleRecievedPacket(input)
    }
}

private object ClientHandler extends IClientPacketHandler
{
    override def handlePacket(packet: PacketCustom, mc: Minecraft, handler: IClientPlayNetHandler):Unit = {
        packet.getType match {
            case TILE_UPDATE_FROM_SERVER => handleTileUpdate(mc.level, packet)
            case NET_TILE_PACKET_TO_CLIENT => handlePacketReceiverTileClientPacket(mc.level, packet)
            case _ =>
        }
    }

    private def handlePacketReceiverTileClientPacket(world:World, input:MCDataInput):Unit = {
        val tile = world.getBlockEntity(input.readPos())
        val key = input.readInt()
        if (tile != null && tile.isInstanceOf[IPacketReceiverTile])
            tile.asInstanceOf[IPacketReceiverTile].receiveUpdateFromServer(key, input)
    }
}

private object ServerHandler extends IServerPacketHandler
{
    override def handlePacket(packet: PacketCustom, sender: ServerPlayerEntity, handler: IServerPlayNetHandler):Unit = {
        packet.getType match  {
            case TILE_UPDATE_FROM_CLIENT => handleTileUpdate(sender.level, packet)
            case NET_TILE_PACKET_TO_SERVER => handlePacketReceiverTileClientPacket(sender.level, packet, sender);
            case _ =>
        }
    }

    private def handlePacketReceiverTileClientPacket(world:World, input:MCDataInput, sender:ServerPlayerEntity):Unit = {
        val tile = world.getBlockEntity(input.readPos())
        val key = input.readInt()
        if (tile != null && tile.isInstanceOf[IPacketReceiverTile])
            tile.asInstanceOf[IPacketReceiverTile].receiveUpdateFromClient(key, input, sender)
    }
}
