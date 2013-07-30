package mrtjp.projectred.integration;

import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.network.NetServerHandler;
import net.minecraft.world.WorldServer;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.packet.PacketCustom.IServerPacketHandler;

public class IntegrationSPH implements IServerPacketHandler {

	@Override
	public void handlePacket(PacketCustom packet, NetServerHandler nethandler, EntityPlayerMP player) {
		WorldServer world = (WorldServer) player.worldObj;
		
		switch (packet.getType()) {
		case IntegrationNetworkConstants.guiButtonPressed:
			if (player.openContainer instanceof ContainerMultipart) {
				((ContainerMultipart)player.openContainer).handleActionFromClient(packet.readByte());
			}
		}	
	}
}
