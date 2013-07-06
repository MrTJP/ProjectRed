package mrtjp.projectred.network.packets;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import mrtjp.projectred.network.abstractpackets.CoordinatesPacket;
import mrtjp.projectred.network.abstractpackets.ModernPacket;
import mrtjp.projectred.tiles.TileAlloySmelter;
import mrtjp.projectred.utils.BasicUtils;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;

public class AlloySmelterUpdatePacket extends CoordinatesPacket {
		
	
	public int heat;
	public int progress;
		
	public AlloySmelterUpdatePacket(int id) {
		super(id);
	}

	@Override
	public ModernPacket template() {
		return new AlloySmelterUpdatePacket(getID());
	}

	@Override
	public void processPacket(EntityPlayer player) {
		TileAlloySmelter tile = getTile(player.worldObj, TileAlloySmelter.class);
		if (tile != null) {
			tile.heat = heat;
			tile.progress = progress;
		}
	}

	@Override
	public void writeData(DataOutputStream data) throws IOException {
		super.writeData(data);
		data.writeInt(heat);
		data.writeInt(progress);
	}

	@Override
	public void readData(DataInputStream data) throws IOException {
		super.readData(data);
		heat = data.readInt();
		progress = data.readInt();
	}
}
