package mrtjp.projectred.illumination;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.Coords;
import mrtjp.projectred.transmission.BasicWireUtils;
import net.minecraft.block.Block;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.network.INetworkManager;
import net.minecraft.network.packet.Packet;
import net.minecraft.network.packet.Packet132TileEntityData;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.AxisAlignedBB;
import net.minecraftforge.common.ForgeDirection;

public class TileLantern extends TileEntity {
	public boolean inverted;
	public boolean powered;
	public boolean updateNextTick = true;
	public boolean updateStateNextTick = true;
	public int lanternmeta;
	public int rotation = 1;
	public boolean initialized = false;

	public TileLantern() {
	}

	public TileLantern(int meta, boolean inv, int rot) {
		lanternmeta = meta;
		inverted = inv;
		rotation = rot;
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
		checkSupport();
		updateNextTick = true;
		updateStateNextTick = true;
	}

	/**
	 * See if the lamp is still attached to something.
	 */
	public void checkSupport() {
		if (BasicUtils.isClient(worldObj) || !initialized) {
			return;
		}
		Coords localCoord = new Coords(this);
		localCoord.orientation = ForgeDirection.getOrientation(this.rotation);
		localCoord.moveForwards(1);
		Block supporter = Block.blocksList[worldObj.getBlockId(localCoord.x, localCoord.y, localCoord.z)];
		if (!BasicWireUtils.canPlaceWireOnSide(worldObj, localCoord.x, localCoord.y, localCoord.z, localCoord.orientation.getOpposite(), false)) {
			worldObj.setBlockToAir(xCoord, yCoord, zCoord);
			BasicUtils.dropItem(worldObj, xCoord, yCoord, zCoord, getDroppedBlock());
			worldObj.removeBlockTileEntity(xCoord, yCoord, zCoord);
		}
	}

	/**
	 * This is called to update on or off state. Usually called when neighboring
	 * block changes, and this rechecks redstone inputs. It sets the powered
	 * flag to its correct state.
	 */
	public void updateState() {
		if(worldObj.isRemote)
			return;
		
		if (isBeingPowered()) {
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
	
	private boolean isBeingPowered() {
		return worldObj.isBlockIndirectlyGettingPowered(xCoord, yCoord, zCoord);
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

		if (!initialized) {
			initialized = true;
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
		nbt.setInteger("rot", rotation);
		nbt.setBoolean("powered", powered);
	}

	/**
	 * Reads a tile entity from NBT.
	 */
	@Override
	public void readFromNBT(NBTTagCompound nbt) {
		super.readFromNBT(nbt);
		inverted = nbt.getBoolean("inverted");
		lanternmeta = nbt.getInteger("meta");
		rotation = nbt.getInteger("rot");
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

	/**
	 * We need the entire model to render, because the stand is not in the BBox.
	 */
	@Override
	public AxisAlignedBB getRenderBoundingBox() {
		return AxisAlignedBB.getAABBPool().getAABB(xCoord, yCoord, zCoord, xCoord + 1, yCoord + 1, zCoord + 1);	}

}
