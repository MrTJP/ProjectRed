package mrtjp.projectred.core;

import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.NetClientHandler;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.world.World;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.packet.PacketCustom.IClientPacketHandler;
import codechicken.lib.vec.BlockCoord;

public class CoreCPH implements IClientPacketHandler {

    @Override
    public void handlePacket(PacketCustom packetCustom, NetClientHandler nethandler, Minecraft mc) {
        EntityPlayer player = mc.thePlayer;
        World world = mc.theWorld;

        switch (packetCustom.getType()) {
        case CoreProxy.messengerQueue:
            Messenger.addMessage(packetCustom.readFloat(), packetCustom.readFloat(), packetCustom.readFloat(), packetCustom.readString());
        }
    }

}
