package mrtjp.projectred.core;

import mrtjp.projectred.ProjectRedCore;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.NetClientHandler;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.world.World;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.packet.PacketCustom.IClientPacketHandler;

public class CoreCPH implements IClientPacketHandler {
    public static Object channel = ProjectRedCore.instance;

    @Override
    public void handlePacket(PacketCustom packet, NetClientHandler nethandler, Minecraft mc) {
        EntityPlayer player = mc.thePlayer;
        World world = mc.theWorld;

        switch (packet.getType()) {
        case CoreSPH.messengerQueue:
            Messenger.addMessage(packet.readDouble(), packet.readDouble(), packet.readDouble(), packet.readString());
            return;
        case CoreSPH.alloySmelterWatcherUpdate:
            TileAlloySmelter t = BasicUtils.getTileEntity(world, packet.readCoord(), TileAlloySmelter.class);
            if (t != null) {
                t.heat = packet.readShort();
                t.progress = packet.readShort();
            }
            return;
        }
    }

}
