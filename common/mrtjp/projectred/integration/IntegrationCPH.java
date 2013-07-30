package mrtjp.projectred.integration;

import mrtjp.projectred.utils.BasicGuiUtils;
import mrtjp.projectred.utils.BasicUtils;
import mrtjp.projectred.utils.Coords;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.NetClientHandler;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.world.World;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.packet.PacketCustom.IClientPacketHandler;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.TileMultipart;

public class IntegrationCPH implements IClientPacketHandler {

	@Override
	public void handlePacket(PacketCustom packet, NetClientHandler nethandler, Minecraft mc) {
		EntityPlayer player = mc.thePlayer;
		World world = mc.theWorld;

		switch (packet.getType()) {
		case IntegrationNetworkConstants.guiTimerOpen:
			int windowID = packet.readInt();
			BlockCoord b = packet.readCoord();
			int side = packet.readByte();
			TileMultipart tmp = (TileMultipart) BasicUtils.getTileEntity(world, new Coords(b.x, b.y, b.z), TileMultipart.class);
			if (tmp != null) {
				BasicGuiUtils.openSMPGui(windowID, new GuiTimer(new ContainerTimer(player, tmp.partMap(side))));
			}
		}
	}
}
