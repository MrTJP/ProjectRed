package mrtjp.projectred.core.tile;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.packet.PacketCustom;
import mrtjp.projectred.core.CoreNetwork;
import net.minecraft.entity.player.ServerPlayerEntity;

import java.util.Collection;
import java.util.function.Consumer;

public interface IPacketReceiverTile extends IBlockEventTile {

    default void sendUpdateToServer(int key, Consumer<MCDataOutput> writer) {
        PacketCustom packet = CoreNetwork.createPacketReceiverTileServerPacket(this, key);
        writer.accept(packet);
        packet.sendToServer();
    }

    default void sendUpdateToPlayersWatchingChunk(int key, Consumer<MCDataOutput> writer) {
        PacketCustom packet = CoreNetwork.createPacketReceiverTileClientPacket(this, key);
        writer.accept(packet);
        packet.sendToChunk(getBlockLevel(), getBlockPosition());
    }

    default void sendUpdateToPlayer(int key, Consumer<MCDataOutput> writer, ServerPlayerEntity player) {
        PacketCustom packet = CoreNetwork.createPacketReceiverTileClientPacket(this, key);
        writer.accept(packet);
        packet.sendToPlayer(player);
    }

    default void sendUpdateToPlayerList(int key, Consumer<MCDataOutput> writer, Collection<ServerPlayerEntity> players) {
        PacketCustom packet = CoreNetwork.createPacketReceiverTileClientPacket(this, key);
        writer.accept(packet);
        for (ServerPlayerEntity player : players) {
            packet.sendToPlayer(player);
        }
    }

    void receiveUpdateFromServer(int key, MCDataInput input);

    void receiveUpdateFromClient(int key, MCDataInput input, ServerPlayerEntity player);
}
