package mrtjp.projectred.expansion;

import codechicken.lib.packet.ICustomPacketHandler;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.packet.PacketCustomChannel;
import net.minecraft.client.Minecraft;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.neoforged.bus.api.IEventBus;

import java.util.Objects;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;

public class ExpansionNetwork {

    public static final ResourceLocation NET_CHANNEL = new ResourceLocation(MOD_ID, "network");

    // Server to client messages
    public static final int MM_FROM_SERVER = 1;
    public static final int LINK_DEBUG_RENDERER_FROM_SERVER = 2;

    // Channel instance
    private static final PacketCustomChannel channel = new PacketCustomChannel(NET_CHANNEL)
            .versioned(ProjectRedExpansion.getContainer().getModInfo().getVersion().toString())
            .client(() -> ClientHandler::new)
            .server(() -> ServerHandler::new);


    public static void init(IEventBus modBus) {
        channel.init(modBus);
    }

    private static class ClientHandler implements ICustomPacketHandler.IClientPacketHandler {

        @Override
        public void handlePacket(PacketCustom packet, Minecraft mc) {
            switch (packet.getType()) {
                case MM_FROM_SERVER -> MovementManager.getInstance(Objects.requireNonNull(mc.level)).read(packet, mc.level);
                case LINK_DEBUG_RENDERER_FROM_SERVER -> GraphDebugManager.getInstance(Objects.requireNonNull(mc.level)).read(packet, mc.level);
                default -> throw new RuntimeException("Invalid key received from server: " + packet.getType());
            }
        }
    }

    private static class ServerHandler implements ICustomPacketHandler.IServerPacketHandler {

        @Override
        public void handlePacket(PacketCustom packet, ServerPlayer sender) {

        }
    }
}
