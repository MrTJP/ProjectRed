package mrtjp.projectred.network.packets;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import mrtjp.projectred.interfaces.ISyncedContainer;
import mrtjp.projectred.network.abstractpackets.ModernPacket;
import net.minecraft.entity.player.EntityPlayer;

public class ContainersButtonPressedPacket extends ModernPacket {
	
	public int buttonID;
	
	public void setButtonID(int id) {
		buttonID = id;
	}
	public ContainersButtonPressedPacket(int id) {
		super(id);
	}

	@Override
	public void readData(DataInputStream data) throws IOException {
		buttonID = data.readInt();
	}

	@Override
	public void processPacket(EntityPlayer player) {
		if (player != null) {
			if (player.openContainer instanceof ISyncedContainer) {
				((ISyncedContainer) player.openContainer).onButtonPressed(buttonID);
			}
		}
	}

	@Override
	public void writeData(DataOutputStream data) throws IOException {
		data.writeInt(buttonID);
	}

	@Override
	public ModernPacket template() {
		return new ContainersButtonPressedPacket(getID());
	}

}
