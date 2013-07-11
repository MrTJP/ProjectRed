package mrtjp.projectred.network.packets;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import mrtjp.projectred.network.abstractpackets.CoordinatesPacket;
import mrtjp.projectred.network.abstractpackets.ModernPacket;
import mrtjp.projectred.tiles.TileLamp;
import net.minecraft.entity.player.EntityPlayer;

public class LampUpdatePacket extends CoordinatesPacket {
		
	
	public boolean isInverted;
	public boolean powered;
	
	public LampUpdatePacket(int id) {
		super(id);
	}

	@Override
	public ModernPacket template() {
		return new LampUpdatePacket(getID());
	}

	@Override
	public void processPacket(EntityPlayer player) {
		TileLamp tile = getTile(player.worldObj, TileLamp.class);
		if (tile != null) {
			tile.inverted = isInverted;
			tile.updateStateNextTick = true;
			tile.updateNextTick = true;
			tile.powered = powered;
			System.err.println("LampUpdatePacket");
		}
	}

	@Override
	public void writeData(DataOutputStream data) throws IOException {
		super.writeData(data);
		data.writeBoolean(isInverted);
		data.writeBoolean(powered);
	}

	@Override
	public void readData(DataInputStream data) throws IOException {
		super.readData(data);
		isInverted = data.readBoolean();
		powered = data.readBoolean();
	}
}
