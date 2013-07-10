package mrtjp.projectred.network.packets;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import mrtjp.projectred.network.abstractpackets.CoordinatesPacket;
import mrtjp.projectred.network.abstractpackets.ModernPacket;
import mrtjp.projectred.tiles.TileLantern;
import mrtjp.projectred.utils.BasicUtils;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.nbt.NBTTagCompound;

public class LanternUpdatePacket extends CoordinatesPacket {
		
	
	public NBTTagCompound lanternData;
	
	public LanternUpdatePacket(int id) {
		super(id);
	}

	@Override
	public ModernPacket template() {
		return new LanternUpdatePacket(getID());
	}

	@Override
	public void processPacket(EntityPlayer player) {
		TileLantern tile = getTile(player.worldObj, TileLantern.class);
		if (tile != null) {
			tile.readFromNBT(lanternData);
		}
	}

	@Override
	public void writeData(DataOutputStream data) throws IOException {
		super.writeData(data);
		BasicUtils.writeNBTToData(lanternData, data);
	}

	@Override
	public void readData(DataInputStream data) throws IOException {
		super.readData(data);
		lanternData = (NBTTagCompound) BasicUtils.readNBTFromData(data);
	}
}
