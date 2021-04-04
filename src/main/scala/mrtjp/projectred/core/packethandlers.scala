package mrtjp.projectred.core

import codechicken.lib.data.MCDataInput
import codechicken.lib.packet.ICustomPacketHandler.{IClientPacketHandler, IServerPacketHandler}
import codechicken.lib.packet.{PacketCustom, PacketCustomChannelBuilder}
import codechicken.lib.util.CrashLock
import mrtjp.projectred.ProjectRedCore
import mrtjp.projectred.core.CoreNetwork._
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

    def init() {
        LOCK.lock()
        PacketCustomChannelBuilder.named(NET_CHANNEL)
                .assignClientHandler(() => () => ClientHandler)
                .assignServerHandler(() => () => ServerHandler)
                .build()
    }


    def createUpdatePacket(tile:CoreTile):PacketCustom = {
        val packet = new PacketCustom(NET_CHANNEL,
            if (!tile.getWorld.isRemote) TILE_UPDATE_FROM_SERVER else TILE_UPDATE_FROM_CLIENT)
        packet.writePos(tile.getPos)
        packet
    }

    private[core] def handleTileUpdate(world:World, input:MCDataInput):Unit = {
        val tile = world.getTileEntity(input.readPos())
        if (tile != null && tile.isInstanceOf[CoreTile])
            tile.asInstanceOf[CoreTile].handleRecievedPacket(input)
    }
}

private object ClientHandler extends IClientPacketHandler
{
    override def handlePacket(packet: PacketCustom, mc: Minecraft, handler: IClientPlayNetHandler):Unit = {
        packet.getType match {
            case TILE_UPDATE_FROM_SERVER => handleTileUpdate(mc.world, packet)
            case _ =>
        }
    }
}

private object ServerHandler extends IServerPacketHandler
{
    override def handlePacket(packet: PacketCustom, sender: ServerPlayerEntity, handler: IServerPlayNetHandler):Unit = {
        packet.getType match  {
            case TILE_UPDATE_FROM_CLIENT => handleTileUpdate(sender.world, packet)
            case _ =>
        }
    }
}
