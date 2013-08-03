package mrtjp.projectred.illumination;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.network.INetworkManager;
import net.minecraft.network.packet.Packet;
import net.minecraft.network.packet.Packet132TileEntityData;
import net.minecraft.tileentity.TileEntity;

public class TileLamp extends TileEntity {

	public boolean inverted;
	public boolean powered;
	public boolean updateNextTick = true;
	public boolean updateStateNextTick = true;
	
	public TileLamp(boolean isInverted) {
		inverted = isInverted;
	}

	public TileLamp() {
	}
			
	/**
	 * Returns the LampType ordinal of the correct color. This will only equal
	 * the actual meta if the lamp is NOT inverted. Use getTrueMeta() to get the
	 * actual meta.
	 * 
	 * @return
	 */
	public int getColor() {
		return worldObj.getBlockMetadata(xCoord, yCoord, zCoord);
	}

	/**
	 * Provides the actual meta of the tile and the block, since for some
	 * reason, only metas up to 15 are placeable in world.
	 * 
	 * @return
	 */
	public int getTrueMeta() {
		return getColor() + (inverted ? 16 : 0);
	}

	/**
	 * The block uses this to determine the light value to emmit.
	 * 
	 * @return
	 */
	public int getLightValue() {
		if (powered != inverted) {
			return 15;
		} else {
			return 0;
		}
	}

	/**
	 * 
	 * @return The Itemstack that should be dropped when the block is broken.
	 */
	public ItemStack getDroppedBlock() {
		return new ItemStack(worldObj.getBlockId(xCoord, yCoord, zCoord), 1, worldObj.getBlockMetadata(xCoord, yCoord, zCoord) + (inverted ? 16 : 0));
	}

	/**
	 * When a neighbor changes, there is a possibility that it was the redstone
	 * signal. The state should be checked.
	 */
	public void onNeighborBlockChange() {
		updateNextTick = true;
		updateStateNextTick = true;
	}

	/**
	 * This is called to update on or off state. Usually called when neighboring
	 * block changes, and this rechecks redstone inputs. It sets the powered
	 * flag to its correct state.
	 */
	public void updateState() {
		// we can't rely on isBlockIndirectlyGettingPowered server side (??), so use the powered state in the packet
		if(worldObj.isRemote)
			return;
		
		if (isBeingPowered()) {
			if(powered)
				return;
			powered = true;
			updateNextTick = true;
		} else {
			if(!powered)
				return;
			powered = false;
			updateNextTick = true;
		}		
	}

	/**
	 * Allows the entity to update its state. Overridden in most subclasses,
	 * e.g. the mob spawner uses this to count ticks and creates a new spawn
	 * inside its implementation.
	 */
	@Override
	public void updateEntity() {
		if (updateStateNextTick) {
			updateStateNextTick = false;
			updateState();
		}
		if (updateNextTick) {
			updateNextTick = false;
			worldObj.markBlockForUpdate(xCoord, yCoord, zCoord);
			worldObj.updateAllLightTypes(xCoord, yCoord, zCoord);
		}
	}

	/**
	 * Writes a tile entity from NBT.
	 */
	@Override
	public void writeToNBT(NBTTagCompound nbt) {
		super.writeToNBT(nbt);
		nbt.setBoolean("inverted", inverted);
		nbt.setBoolean("powered", powered);
	}

	/**
	 * Reads a tile entity from NBT.
	 */
	@Override
	public void readFromNBT(NBTTagCompound nbt) {
		super.readFromNBT(nbt);
		inverted = nbt.getBoolean("inverted");
		powered = nbt.getBoolean("powered");
		updateStateNextTick = true;
		updateNextTick = true;
	}

	/**
	 * Pushes the state to the client.
	 */
	@Override
	public Packet getDescriptionPacket() {
		NBTTagCompound nbt = new NBTTagCompound();
		writeToNBT(nbt);
		return new Packet132TileEntityData(xCoord, yCoord, zCoord, 1, nbt);
	}
	
	@Override
	public void onDataPacket(INetworkManager net, Packet132TileEntityData pkt)
	{
		readFromNBT(pkt.customParam1);
	}
	
	private boolean isBeingPowered() {
		return worldObj.isBlockIndirectlyGettingPowered(xCoord, yCoord, zCoord);
	}
}
