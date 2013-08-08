package mrtjp.projectred.integration;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.multiplayer.NetClientHandler;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.world.World;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.packet.PacketCustom.IClientPacketHandler;
import codechicken.lib.vec.BlockCoord;

public class IntegrationCPH implements IClientPacketHandler {

    @Override
    public void handlePacket(PacketCustom packet, NetClientHandler nethandler, Minecraft mc) {
        EntityPlayer player = mc.thePlayer;
        World world = mc.theWorld;

        switch (packet.getType()) {
        case IntegrationNetworkConstants.guiTimerOpen:
            guiTimerUpdate(packet, player, true);
            break;
        case IntegrationNetworkConstants.guiTimerBroadcastChange:
            guiTimerUpdate(packet, player, false);
            break;
        case IntegrationNetworkConstants.guiCounterOpen:
            guiCounterUpdate(packet, player, true);
            break;
        case IntegrationNetworkConstants.guiCounterBroadcastChange:
            guiCounterUpdate(packet, player, false);
            break;
        }
    }

    public void guiTimerUpdate(PacketCustom packet, EntityPlayer p, boolean openNew) {
        BlockCoord b = packet.readCoord();
        int face = packet.readByte();
        int interval = packet.readInt();
        if (openNew) {
            GuiTimer t = new GuiTimer();
            t.coords = b;
            t.face = face;
            t.timerInterval = interval;
            Minecraft.getMinecraft().displayGuiScreen(t);
        } else {
            GuiScreen g = Minecraft.getMinecraft().currentScreen;
            if (g instanceof GuiTimer) {
                GuiTimer tg = (GuiTimer) g;
                if (tg.coords.equals(b)) {
                    tg.face = face;
                    tg.timerInterval = interval;
                }
            }
        }
    }
    
    public void guiCounterUpdate(PacketCustom packet, EntityPlayer p, boolean openNew) {
        BlockCoord b = packet.readCoord();
        int face = packet.readByte();
        int value = packet.readShort();
        int max = packet.readShort();
        int incr = packet.readShort();
        int decr = packet.readShort();
        if (openNew) {
            GuiCounter c = new GuiCounter();
            c.coords = b;
            c.face = face;
            c.value = value;
            c.max = max;
            c.incr = incr;
            c.decr = decr;
            Minecraft.getMinecraft().displayGuiScreen(c);
        } else {
            GuiScreen g = Minecraft.getMinecraft().currentScreen;
            if (g instanceof GuiCounter) {
                GuiCounter cg = (GuiCounter) g;
                if (cg.coords.equals(b)) {
                    cg.face = face;
                    cg.value = value;
                    cg.max = max;
                    cg.incr = incr;
                    cg.decr = decr;
                }
            }
        }
    }

}
