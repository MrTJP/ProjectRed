package mrtjp.projectred.expansion;

import codechicken.lib.packet.ICustomPacketHandler;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.packet.PacketCustomChannelBuilder;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.ClientPacketListener;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.server.network.ServerGamePacketListenerImpl;

import java.util.Objects;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;

public class ExpansionNetwork {

    public static final ResourceLocation NET_CHANNEL = new ResourceLocation(MOD_ID, "network");

    // Server to client messages
    public static final int MM_FROM_SERVER = 1;
    public static final int LINK_DEBUG_RENDERER_FROM_SERVER = 2;

    public static void init() {
        PacketCustomChannelBuilder.named(NET_CHANNEL)
                .assignClientHandler(() -> ClientHandler::new)
                .assignServerHandler(() -> ServerHandler::new)
                .build();
    }

    private static class ClientHandler implements ICustomPacketHandler.IClientPacketHandler {

        @Override
        public void handlePacket(PacketCustom packet, Minecraft mc, ClientPacketListener handler) {
            switch (packet.getType()) {
                case MM_FROM_SERVER -> MovementManager.getInstance(Objects.requireNonNull(mc.level)).read(packet, mc.level);
                case LINK_DEBUG_RENDERER_FROM_SERVER -> GraphDebugManager.getInstance(Objects.requireNonNull(mc.level)).read(packet, mc.level);
                default -> throw new RuntimeException("Invalid key received from server: " + packet.getType());
            }
        }
    }

    private static class ServerHandler implements ICustomPacketHandler.IServerPacketHandler {

        @Override
        public void handlePacket(PacketCustom packet, ServerPlayer sender, ServerGamePacketListenerImpl handler) {

        }
    }
}
