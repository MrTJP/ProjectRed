package mrtjp.projectred.interfaces;

import net.minecraft.nbt.NBTTagCompound;

public interface ISaveState {
	/**
	 * Called to read every information for the given class from the NBTTagCompount
	 * @param nbttagcompound to read from
	 */
	public void readFromNBT(NBTTagCompound nbttagcompound);
	
	/**
	 * Called to save all information of the given class into an NBTTagCompount
	 * @param nbttagcompound to save the information in
	 */
	public void writeToNBT(NBTTagCompound nbttagcompound);
}
