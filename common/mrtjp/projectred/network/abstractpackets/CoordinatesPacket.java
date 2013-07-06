package mrtjp.projectred.network.abstractpackets;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import net.minecraft.tileentity.TileEntity;
import net.minecraft.world.World;

public abstract class CoordinatesPacket extends ModernPacket {

	public CoordinatesPacket(int id) {
		super(id);
	}

	public int posX;
	public int posY;
	public int posZ;
		
	public int getPosX() {
		return posX;
	}
	public int getPosY() {
		return posY;
	}
	public int getPosZ() {
		return posZ;
	}



	@Override
	public void writeData(DataOutputStream data) throws IOException {

		data.writeInt(posX);
		data.writeInt(posY);
		data.writeInt(posZ);
	}

	@Override
	public void readData(DataInputStream data) throws IOException {

		posX = data.readInt();
		posY = data.readInt();
		posZ = data.readInt();

	}

	/**
	 * Retrieves tileEntity at packet coordinates if any.
	 * 
	 * @param world
	 * @param clazz
	 * @return TileEntity
	 */
	public <T extends TileEntity> T getTile(World world, Class<T> clazz) {
		if (world == null) {
			return null;
		}
		if (!world.blockExists(posX, posY, posZ)) {
			return null;
		}

		final TileEntity tile = world.getBlockTileEntity(posX, posY, posZ);
		if (tile == null) {
			return null;
		}
		if (!(clazz.isAssignableFrom(tile.getClass()))) {
			return null;
		}
		return (T) tile;
	}
}
