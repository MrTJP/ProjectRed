package mrtjp.projectred.tiles;

import net.minecraft.entity.EntityLivingBase;
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

	public void onBlockPlaced(EntityLivingBase player, ItemStack item) {
		int entityrotation = MathHelper.floor_double((double) ((player.rotationYaw * 4F) / 360F) + 2.5D) & 3;
		if (entityrotation == 0) {
			rotation = 2;
		} else if (entityrotation == 2) {
			rotation = 3;
		} else if (entityrotation == 3) {
			rotation = 4;
		} else if (entityrotation == 1) {
			rotation = 5;
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
