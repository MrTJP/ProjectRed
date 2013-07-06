package mrtjp.projectred.network.packets;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import mrtjp.projectred.network.abstractpackets.CoordinatesPacket;
import mrtjp.projectred.network.abstractpackets.ModernPacket;
import mrtjp.projectred.tiles.TileLamp;
import net.minecraft.entity.player.EntityPlayer;

public class GateTimerUpdatePacket extends CoordinatesPacket {
		
	
	public boolean isInverted;
	
	public GateTimerUpdatePacket(int id) {
		super(id);
	}

	@Override
	public ModernPacket template() {
		return new GateTimerUpdatePacket(getID());
	}

	@Override
	public void processPacket(EntityPlayer player) {
		TileLamp tile = getTile(player.worldObj, TileLamp.class);
		if (tile != null) {
			tile.inverted = isInverted;
			tile.updateStateNextTick = true;
			tile.updateNextTick = true;
		}
	}

	@Override
	public void writeData(DataOutputStream data) throws IOException {
		super.writeData(data);
		data.writeBoolean(isInverted);
	}

	@Override
	public void readData(DataInputStream data) throws IOException {
		super.readData(data);
		isInverted = data.readBoolean();
	}
}
