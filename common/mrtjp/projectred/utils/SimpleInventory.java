/** 
 * Copyright (c) Krapht, 2011
 * 
 * "LogisticsPipes" is distributed under the terms of the Minecraft Mod Public 
 * License 1.0, or MMPL. Please check the contents of the license located in
 * http://www.mod-buildcraft.com/MMPL-1.0.txt
 */

package mrtjp.projectred.utils;

import java.util.LinkedList;

import mrtjp.projectred.interfaces.ISaveState;
import mrtjp.projectred.interfaces.ISimpleInventoryListener;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.IInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.world.World;

public class SimpleInventory implements IInventory {

	private final String _name;
	private ItemStack[] _contents;
	private final int _stackLimit;
	
	private final LinkedList<ISimpleInventoryListener> _listener = new LinkedList<ISimpleInventoryListener>(); 
	
	public SimpleInventory(int size, String name, int stackLimit){
		_contents = new ItemStack[size];
		_name = name;
		_stackLimit = stackLimit;
	}
	
	@Override
	public int getSizeInventory() {
		return _contents.length;
	}

	@Override
	public ItemStack getStackInSlot(int i) {
		return _contents[i];
	}

	@Override
	public ItemStack decrStackSize(int i, int j) {
		if (_contents[i] == null) return null;
		if (_contents[i].stackSize > j) {
			ItemStack ret = _contents[i].splitStack(j);
			return ret;
		}
		ItemStack ret = _contents[i];
		_contents[i] = null;
		return ret;
	}

	@Override
	public void setInventorySlotContents(int i, ItemStack itemstack) {
		_contents[i] = itemstack;
	}

	@Override
	public String getInvName() {
		return _name;
	}

	@Override
	public int getInventoryStackLimit() {
		return _stackLimit;
	}

	@Override
	public void onInventoryChanged() {
		for (ISimpleInventoryListener handler : _listener){
			handler.InventoryChanged(this);
		}
	}

	@Override
	public boolean isUseableByPlayer(EntityPlayer entityplayer) {return false;}

	@Override
	public void openChest() {}

	@Override
	public void closeChest() {}

	public void readFromNBT(NBTTagCompound nbttagcompound) {
		readFromNBT(nbttagcompound, "");
	}
	
	public void readFromNBT(NBTTagCompound nbt, String prefix) {
		if (nbt == null) {
			nbt = new NBTTagCompound();
		}
		NBTTagList nbttaglist = nbt.getTagList(prefix + "items");
    	
    	for (int j = 0; j < nbttaglist.tagCount(); ++j) {    		
    		NBTTagCompound nbttagcompound2 = (NBTTagCompound) nbttaglist.tagAt(j);
    		int index = nbttagcompound2.getInteger("index");
    		if(index < _contents.length) {
    			_contents [index] = ItemStack.loadItemStackFromNBT(nbttagcompound2);
    		}
    	}
    	onInventoryChanged();
	}

	public void writeToNBT(NBTTagCompound nbttagcompound) {
		writeToNBT(nbttagcompound, "");
	}

	public void writeToNBT(NBTTagCompound nbt, String prefix) {
		if (nbt == null) {
			nbt = new NBTTagCompound();
		}
		NBTTagList nbttaglist = new NBTTagList();
    	for (int j = 0; j < _contents.length; ++j) {    		    		
    		if (_contents[j] != null && _contents[j].stackSize > 0) {
        		NBTTagCompound nbttagcompound2 = new NBTTagCompound ();
        		nbttaglist.appendTag(nbttagcompound2);
    			nbttagcompound2.setInteger("index", j);
    			_contents[j].writeToNBT(nbttagcompound2);	
    		}     		
    	}
    	nbt.setTag(prefix + "items", nbttaglist);
    	nbt.setInteger(prefix + "itemsCount", _contents.length);
	}

	public void dropContents(World worldObj, int posX, int posY, int posZ) {
		if(BasicUtils.isServer(worldObj)) {
			for(int i=0;i<_contents.length;i++) {
				while(_contents[i] != null) {
					ItemStack todrop = decrStackSize(i, _contents[i].getMaxStackSize());
			    	BasicUtils.dropItem(worldObj, posX, posY, posZ, todrop);
				}
			}
	    	onInventoryChanged();
		}
	}
	
	public void addListener(ISimpleInventoryListener listner){
		if (!_listener.contains(listner)){
			_listener.add(listner);
		}
	}
	
	public void removeListener(ISimpleInventoryListener listner){
		if (_listener.contains(listner)){
			_listener.remove(listner);
		}
	}

	@Override
	public ItemStack getStackInSlotOnClosing(int i) {
		if (this._contents[i] == null) return null;
		
		ItemStack stackToTake = this._contents[i];
		this._contents[i] = null;
    	onInventoryChanged();
		return stackToTake;
	}
	
	private int tryAddToSlot(int i, ItemStack stack) {
		ItemStack slot = _contents[i];
		if(slot == null) {
			_contents[i] = stack.copy();
			return stack.stackSize;
		}
		if(BasicUtils.areStacksTheSame(slot, stack)) {
			slot.stackSize += stack.stackSize;
			if(slot.stackSize > 127) {
				int ans = stack.stackSize - (slot.stackSize - 127);
				slot.stackSize = 127;
				return ans;
			} else {
				return stack.stackSize;
			}
		} else {
			return 0;
		}
	}
	
	public int addCompressed(ItemStack stack) {
		if(stack == null) return 0;
		stack = stack.copy();
		for(int i=0; i<this._contents.length;i++) {
			if(stack.stackSize <= 0) {
				break;
			}
			if(_contents[i] == null) continue; //Skip Empty Slots on first attempt.
			int added = tryAddToSlot(i, stack);
			stack.stackSize -= added;
		}
		for(int i=0; i<this._contents.length;i++) {
			if(stack.stackSize <= 0) {
				break;
			}
			int added = tryAddToSlot(i, stack);
			stack.stackSize -= added;
		}
		onInventoryChanged();
		return stack.stackSize;
	}



	@Override
	public boolean isInvNameLocalized() {
		return true;
	}

	@Override
	public boolean isStackValidForSlot(int i, ItemStack itemstack) {
		return false;
	}
	
	public ItemStack[] getContents() {
		return this._contents;
	}
}
