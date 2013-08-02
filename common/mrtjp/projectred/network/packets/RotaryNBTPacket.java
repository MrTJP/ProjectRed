package mrtjp.projectred.network.packets;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import mrtjp.projectred.network.abstractpackets.CoordinatesPacket;
import mrtjp.projectred.network.abstractpackets.ModernPacket;
import mrtjp.projectred.tiles.TileTurbineRotary;
import mrtjp.projectred.utils.BasicUtils;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.nbt.NBTTagCompound;

public class RotaryNBTPacket extends CoordinatesPacket {
		
	public NBTTagCompound tiledata;
		
	public RotaryNBTPacket(int id) {
		super(id);
	}

	@Override
	public ModernPacket template() {
		return new RotaryNBTPacket(getID());
	}

	@Override
	public void processPacket(EntityPlayer player) {
		TileTurbineRotary tile = getTile(player.worldObj, TileTurbineRotary.class);
		if (tile != null) {
			tile.readFromNBT(tiledata);
			tile.updateNextTick = true;
		}
	}

	@Override
	public void writeData(DataOutputStream data) throws IOException {
		super.writeData(data);
		BasicUtils.writeNBTToData(tiledata, data);
	}

	@Override
	public void readData(DataInputStream data) throws IOException {
		super.readData(data);
		tiledata = (NBTTagCompound)BasicUtils.readNBTFromData(data);
	}
}
