package mrtjp.projectred.illumination;

import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.NetClientHandler;
import mrtjp.projectred.ProjectRedIllumination;
import mrtjp.projectred.core.BasicUtils;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.packet.PacketCustom.IClientPacketHandler;

public class IlluminationCPH implements IClientPacketHandler {
    public static Object channel = ProjectRedIllumination.instance;

	@Override
	public void handlePacket(PacketCustom packet, NetClientHandler nethandler, Minecraft mc) {
        switch (packet.getType()) {
        case 1:
        	TileLamp t = BasicUtils.getTileEntity(mc.theWorld, packet.readCoord(), TileLamp.class);
        	if (t != null) {
        		t.inverted = packet.readBoolean();
        		t.powered = packet.readBoolean();
        		t.color = packet.readByte();
        		t.updateState(true);
        	}
            break;
    	}
	}

}
