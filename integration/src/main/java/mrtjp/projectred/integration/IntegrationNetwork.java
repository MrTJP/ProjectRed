package mrtjp.projectred.integration;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.packet.ICustomPacketHandler;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.packet.PacketCustomChannelBuilder;
import codechicken.multipart.api.part.MultiPart;
import codechicken.multipart.block.BlockMultipart;
import mrtjp.projectred.integration.gui.screen.CounterScreen;
import mrtjp.projectred.integration.gui.screen.TimerScreen;
import mrtjp.projectred.integration.part.ComplexGatePart;
import mrtjp.projectred.integration.part.GatePart;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.ClientPacketListener;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.server.network.ServerGamePacketListenerImpl;
import net.minecraft.world.level.Level;

import java.util.Objects;

import static mrtjp.projectred.integration.ProjectRedIntegration.LOGGER;
import static mrtjp.projectred.integration.ProjectRedIntegration.MOD_ID;

public class IntegrationNetwork {

    public static final ResourceLocation NET_CHANNEL = new ResourceLocation(MOD_ID, "network");

    // Server to client messages
    public static final int OPEN_TIMER_GUI_FROM_SERVER = 1;
    public static final int OPEN_COUNTER_GUI_FROM_SERVER = 2;

    // Client to server messages
    public static final int INCR_TIMER_FROM_CLIENT = 3;
    public static final int INCR_COUNTER_FROM_CLIENT = 4;

    public static void init() {

        PacketCustomChannelBuilder.named(NET_CHANNEL)
                .assignClientHandler(() -> ClientHandler::new)
                .assignServerHandler(() -> ServerHandler::new)
                .build();
    }

    public static MCDataOutput writePartIndex(MCDataOutput out, MultiPart part) {
        out.writePos(part.pos()).writeByte(part.tile().getPartList().indexOf(part));
        return out;
    }

    public static MultiPart readPartIndex(Level world, MCDataInput in) {
        BlockPos pos = in.readPos();
        int index = in.readUByte();
        return Objects.requireNonNull(BlockMultipart.getTile(world, pos)).getPartList().get(index);
    }

    private static class ClientHandler implements ICustomPacketHandler.IClientPacketHandler {

        @Override
        public void handlePacket(PacketCustom packet, Minecraft mc, ClientPacketListener handler) {
            switch (packet.getType()) {
                case OPEN_TIMER_GUI_FROM_SERVER:
                    handleOpenTimerGuiMessage(mc, packet);
                    break;
                case OPEN_COUNTER_GUI_FROM_SERVER:
                    handleOpenCounterGuiMessage(mc, packet);
                    break;
                default:
                    // unknown key
                    throw new RuntimeException("Invalid key received from server: " + packet.getType());
            }
        }

        private void handleOpenTimerGuiMessage(Minecraft mc, MCDataInput data) {
            MultiPart part = readPartIndex(Objects.requireNonNull(mc.level), data);
            if (part instanceof ComplexGatePart.ITimerGuiLogic) {
                mc.setScreen(new TimerScreen((GatePart) part));
            }
        }

        private void handleOpenCounterGuiMessage(Minecraft mc, MCDataInput data) {
            MultiPart part = readPartIndex(Objects.requireNonNull(mc.level), data);
            if (part instanceof ComplexGatePart.ICounterGuiLogic) {
                mc.setScreen(new CounterScreen((GatePart) part));
            }
        }
    }

    private static class ServerHandler implements ICustomPacketHandler.IServerPacketHandler {

        @Override
        public void handlePacket(PacketCustom packet, ServerPlayer sender, ServerGamePacketListenerImpl handler) {
            switch (packet.getType()) {

                case INCR_TIMER_FROM_CLIENT:
                    handleIncrTimerMessage(sender.level(), packet);
                    break;
                case INCR_COUNTER_FROM_CLIENT:
                    handleIncrCounterMessage(sender.level(), packet);
                    break;
                default:
                    // unknown key
                    throw new RuntimeException("Invalid key received from client: " + packet.getType());
            }
        }

        private void handleIncrTimerMessage(Level world, PacketCustom packet) {
            MultiPart part = readPartIndex(world, packet);
            if (part instanceof ComplexGatePart.ITimerGuiLogic gate) {
                gate.setTimerMax(gate.getTimerMax() + packet.readShort());
            }
        }

        private void handleIncrCounterMessage(Level world, PacketCustom packet) {
            MultiPart part = readPartIndex(world, packet);
            if (part instanceof ComplexGatePart.ICounterGuiLogic gate) {
                int actionId = packet.readByte();
                switch (actionId) {
                    case 0:
                        gate.setCounterMax(gate.getCounterMax() + packet.readShort());
                        break;
                    case 1:
                        gate.setCounterIncr(gate.getCounterIncr() + packet.readShort());
                        break;
                    case 2:
                        gate.setCounterDecr(gate.getCounterDecr() + packet.readShort());
                        break;
                    default:
                        LOGGER.error("Unknown counter action id: " + actionId);
                }
            }
        }
    }
}
