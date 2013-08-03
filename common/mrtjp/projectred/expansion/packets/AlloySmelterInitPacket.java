package mrtjp.projectred.expansion.packets;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.expansion.TileAlloySmelter;
import mrtjp.projectred.expansion.abstractpackets.CoordinatesPacket;
import mrtjp.projectred.expansion.abstractpackets.ModernPacket;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.nbt.NBTTagCompound;

public class AlloySmelterInitPacket extends CoordinatesPacket {
		
	public NBTTagCompound tiledata;
		
	public AlloySmelterInitPacket(int id) {
		super(id);
	}

	@Override
	public ModernPacket template() {
		return new AlloySmelterInitPacket(getID());
	}

	@Override
	public void processPacket(EntityPlayer player) {
		TileAlloySmelter tile = getTile(player.worldObj, TileAlloySmelter.class);
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
