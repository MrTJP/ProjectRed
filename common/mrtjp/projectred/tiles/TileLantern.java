package mrtjp.projectred.tiles;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.network.PacketHandler;
import mrtjp.projectred.network.packets.LampUpdatePacket;
import mrtjp.projectred.network.packets.LanternUpdatePacket;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.network.packet.Packet;
import net.minecraft.tileentity.TileEntity;

public class TileLantern extends TileEntity {
	public boolean inverted;
	public boolean powered;
	public boolean updateNextTick;
	public boolean updateStateNextTick;
	public int lanternmeta;

	public TileLantern(){
	}
	
	public TileLantern(int meta, boolean inv) {
		lanternmeta = meta;
		inverted = inv;
	}
	
	public ItemStack getDroppedBlock() {
		return new ItemStack(ProjectRed.blockLantern.blockID, 1, lanternmeta);
	}
	
	public int getLightValue() {
		if (powered != inverted) {
			return 15;
		} else {
			return 0;
		}
	}

	/**
	 * When a neighbor changes, there is a possibility that it was the redstone
	 * signal. The state should be checked.
	 */
	public void onNeighborBlockChange() {
		updateNextTick = true;
		updateStateNextTick = true;
	}

	public void onBlockAdded() {
		updateNextTick = true;
		updateStateNextTick = true;
	}
	
	/**
	 * This is called to update on or off state. Usually called when neighboring
	 * block changes, and this rechecks redstone inputs. It sets the powered
	 * flag to its correct state.
	 */
	public void updateState() {
		boolean isBeingPowered = worldObj.isBlockIndirectlyGettingPowered(xCoord, yCoord, zCoord);
		if (isBeingPowered) {
			if (powered) {
				return;
			}
			powered = true;
			updateNextTick = true;
		} else {
			if (!powered) {
				return;
			}
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
		nbt.setInteger("meta", lanternmeta);
	}

	/**
	 * Reads a tile entity from NBT.
	 */
	@Override
	public void readFromNBT(NBTTagCompound nbt) {
		super.readFromNBT(nbt);
		inverted = nbt.getBoolean("inverted");
		lanternmeta = nbt.getInteger("meta");
		updateStateNextTick = true;
		updateNextTick = true;
	}
	
	/**
	 * Pushes the state to the client.
	 */
	@Override
	public Packet getDescriptionPacket() {
		LanternUpdatePacket packet = PacketHandler.getPacket(LanternUpdatePacket.class);
		packet.posX = xCoord;
		packet.posY = yCoord;
		packet.posZ = zCoord;
		NBTTagCompound nbt = new NBTTagCompound();
		writeToNBT(nbt);
		packet.lanternData = nbt;
		return packet.getPacket();
	}
}
