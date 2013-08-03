package mrtjp.projectred.expansion.abstractpackets;

import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import mrtjp.projectred.core.Configurator;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.network.packet.Packet;
import net.minecraft.network.packet.Packet250CustomPayload;

public abstract class ModernPacket {

	protected String channel;

	public abstract void readData(DataInputStream data) throws IOException;

	public abstract void processPacket(EntityPlayer player);

	public abstract void writeData(DataOutputStream data) throws IOException;

	public Packet getPacket() {

		ByteArrayOutputStream bytes = new ByteArrayOutputStream();
		DataOutputStream data = new DataOutputStream(bytes);
		try {
			data.writeByte(getID());
			writeData(data);
		} catch (IOException e) {
			e.printStackTrace();
		}
		Packet250CustomPayload packet = new Packet250CustomPayload();
		packet.channel = channel;
		packet.data = bytes.toByteArray();
		packet.length = packet.data.length;
		return packet;
	}

	private final int id;

	public ModernPacket(int id) {
		this.channel = Configurator.modNetworkChannel;
		this.id = id;
	}

	public abstract ModernPacket template();

	public int getID() {
		return id;
	}

}