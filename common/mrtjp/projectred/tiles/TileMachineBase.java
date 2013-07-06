package mrtjp.projectred.tiles;

import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.network.packet.Packet;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.MathHelper;

public abstract class TileMachineBase extends TileEntity {

	public int rotation = 2;
	public boolean updateNextTick = false;
	
	public TileMachineBase() {}

	public abstract void onBlockBreak();

	public abstract void onBlockClicked(EntityPlayer player);
	
	public abstract boolean onBlockActivated(EntityPlayer player);

	public void onBlockPlaced(EntityLiving entity, ItemStack item) {
		
		//int dir = MathHelper.floor_double((double)(entity.rotationYaw * 4f / 360f) + 0.5) & 3;
		//setRotation(dir);
		double x = xCoord - entity.posX;
		double z = zCoord - entity.posZ;
		double w = Math.atan2(x, z);
		double halfPI = Math.PI / 2;
		double halfhalfPI = halfPI / 2;
		w -= halfhalfPI;
		if (w < 0) {
			w += 2 * Math.PI;
		}
		if (0 < w && w <= halfPI) {
			setRotation(5); // East
		} else if (halfPI < w && w <= 2 * halfPI) {
			setRotation(2); // North
		} else if (2 * halfPI < w && w <= 3 * halfPI) {
			setRotation(4); // West
		} else if (3 * halfPI < w && w <= 4 * halfPI) {
			setRotation(3); // South
		}
	}

	public void setRotation(int rot) {
		rotation = rot;
	}
	
	@Override
	public void writeToNBT(NBTTagCompound nbt) {
		super.writeToNBT(nbt);
		nbt.setInteger("rotation", rotation);
	}

	@Override
	public void readFromNBT(NBTTagCompound nbt) {
		super.readFromNBT(nbt);
		rotation = nbt.getInteger("rotation");
	}

	@Override
	public abstract Packet getDescriptionPacket();
	
	@Override
	public abstract void updateEntity();
	
	public boolean shouldUseSpecialTextureForSide(int side) {
		return false;
	}
	
	public int getLightLevel() {
		return 0;
	}
}
