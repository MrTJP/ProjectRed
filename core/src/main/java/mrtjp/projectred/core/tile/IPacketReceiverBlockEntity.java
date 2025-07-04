package mrtjp.projectred.core.tile;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.packet.PacketCustom;
import mrtjp.projectred.core.CoreNetwork;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;

import java.util.Collection;
import java.util.function.Consumer;

public interface IPacketReceiverBlockEntity extends IBlockEventBlockEntity {

    default void sendUpdateToServer(int key, Consumer<MCDataOutput> writer) {
        PacketCustom packet = CoreNetwork.createTileServerPacket(this, (byte) key, getBlockLevel().registryAccess());
        writer.accept(packet);
        packet.sendToServer();
    }

    default void sendUpdateToPlayersWatchingChunk(int key, Consumer<MCDataOutput> writer) {
        PacketCustom packet = CoreNetwork.createTileClientPacket(this, (byte) key, getBlockLevel().registryAccess());
        writer.accept(packet);
        packet.sendToChunk((ServerLevel) getBlockLevel(), getBlockPosition());
    }

    default void sendUpdateToPlayer(int key, Consumer<MCDataOutput> writer, ServerPlayer player) {
        PacketCustom packet = CoreNetwork.createTileClientPacket(this, (byte) key, getBlockLevel().registryAccess());
        writer.accept(packet);
        packet.sendToPlayer(player);
    }

    default void sendUpdateToPlayerList(int key, Consumer<MCDataOutput> writer, Collection<ServerPlayer> players) {
        PacketCustom packet = CoreNetwork.createTileClientPacket(this, (byte) key, getBlockLevel().registryAccess());
        writer.accept(packet);
        for (ServerPlayer player : players) {
            packet.sendToPlayer(player);
        }
    }

    void receiveUpdateFromServer(int key, MCDataInput input);

    void receiveUpdateFromClient(int key, MCDataInput input, ServerPlayer player);
}
