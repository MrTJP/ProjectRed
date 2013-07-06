package mrtjp.projectred.network.packets;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import mrtjp.projectred.multipart.BlockMultipartBase;
import mrtjp.projectred.multipart.PartCoordinates;
import mrtjp.projectred.network.abstractpackets.CoordinatesPacket;
import mrtjp.projectred.network.abstractpackets.ModernPacket;
import net.minecraft.entity.player.EntityPlayer;

public class MicroblockBreakingPacket extends CoordinatesPacket {

	public int part;
	public boolean isCSPart;
	
	public void setPart(int param) {
		part = param;
	}
	public void setIsCSPart(boolean param) {
		isCSPart = param;
	}
	
	public MicroblockBreakingPacket(int id) {
		super(id);
	}

	@Override
	public ModernPacket template() {
		return new MicroblockBreakingPacket(getID());
	}

	@Override
	public void processPacket(EntityPlayer player) {
		if(player != null) {
			BlockMultipartBase.setBreakingPart(player, new PartCoordinates(getPosX(), getPosY(), getPosZ(), part, isCSPart));
		}
	}

	@Override
	public void writeData(DataOutputStream data) throws IOException {
		super.writeData(data);
		data.writeInt(part);
		data.writeBoolean(isCSPart);
	}

	@Override
	public void readData(DataInputStream data) throws IOException {
		super.readData(data);
		part = data.readInt();
		isCSPart = data.readBoolean();
	}
}

