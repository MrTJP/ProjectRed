package mrtjp.projectred.core;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.packet.ICustomPacketHandler;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.packet.PacketCustomChannel;
import mrtjp.projectred.core.tile.IPacketReceiverBlockEntity;
import net.minecraft.client.Minecraft;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.neoforged.bus.api.IEventBus;

import java.util.Objects;

import static mrtjp.projectred.core.ProjectRedCore.MOD_ID;

public class CoreNetwork {

    public static final ResourceLocation NET_CHANNEL = new ResourceLocation(MOD_ID, "network");

    // Server to client messages
    public static final int NET_TILE_PACKET_TO_CLIENT = 1;

    // Client to server messages
    public static final int NET_TILE_PACKET_TO_SERVER = 3;

    // Channel instance
    private static final PacketCustomChannel channel = new PacketCustomChannel(NET_CHANNEL)
            .versioned(ProjectRedCore.getContainer().getModInfo().getVersion().toString())
            .client(() -> ClientHandler::new)
            .server(() -> ServerHandler::new);

    public static void init(IEventBus modBus) {
        channel.init(modBus);
    }

    public static PacketCustom createTileClientPacket(IPacketReceiverBlockEntity tile, byte key) {
        PacketCustom packet = new PacketCustom(NET_CHANNEL, NET_TILE_PACKET_TO_CLIENT);
        packet.writePos(tile.getBlockPosition());
        packet.writeByte(key);
        return packet;
    }

    public static PacketCustom createTileServerPacket(IPacketReceiverBlockEntity tile, byte key) {
        PacketCustom packet = new PacketCustom(NET_CHANNEL, NET_TILE_PACKET_TO_SERVER);
        packet.writePos(tile.getBlockPosition());
        packet.writeByte(key);
        return packet;
    }

    private static class ClientHandler implements ICustomPacketHandler.IClientPacketHandler {

        @Override
        public void handlePacket(PacketCustom packet, Minecraft mc) {
            switch (packet.getType()) {
                case NET_TILE_PACKET_TO_CLIENT:
                    handleTilePacket(Objects.requireNonNull(mc.level), packet);
                    break;
                default:
                    // unknown key
                    throw new RuntimeException("Invalid key received from server: " + packet.getType());
            }
        }

        private void handleTilePacket(Level world, MCDataInput data) {
            BlockEntity tile = world.getBlockEntity(data.readPos());
            int key = data.readUByte();
            if (tile instanceof IPacketReceiverBlockEntity)
                ((IPacketReceiverBlockEntity) tile).receiveUpdateFromServer(key, data);
        }
    }

    private static class ServerHandler implements ICustomPacketHandler.IServerPacketHandler {

        @Override
        public void handlePacket(PacketCustom packet, ServerPlayer sender) {
            switch (packet.getType()) {
                case NET_TILE_PACKET_TO_SERVER:
                    handleTilePacket(sender.level(), packet, sender);
                    break;
                default:
                    // unknown key
                    throw new RuntimeException("Invalid key received from client: " + packet.getType());
            }
        }

        private void handleTilePacket(Level world, MCDataInput data, ServerPlayer sender) {
            BlockEntity tile = world.getBlockEntity(data.readPos());
            int key = data.readUByte();
            if (tile instanceof IPacketReceiverBlockEntity)
                ((IPacketReceiverBlockEntity) tile).receiveUpdateFromClient(key, data, sender);
        }
    }
}
