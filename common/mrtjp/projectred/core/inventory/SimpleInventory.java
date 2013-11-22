package mrtjp.projectred.core.inventory;

import mrtjp.projectred.core.BasicUtils;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.IInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.world.World;

public class SimpleInventory implements IInventory {

    private final String invName;
    private ItemStack[] storage;
    private final int slotLimit;

    public SimpleInventory(int size, String name, int stackLimit) {
        storage = new ItemStack[size];
        invName = name;
        slotLimit = stackLimit;
    }

    @Override
    public int getSizeInventory() {
        return storage.length;
    }

    @Override
    public ItemStack getStackInSlot(int i) {
        return storage[i];
    }

    @Override
    public ItemStack decrStackSize(int i, int j) {
        if (storage[i] == null)
            return null;
        if (storage[i].stackSize > j) {
            ItemStack ret = storage[i].splitStack(j);
            return ret;
        }
        ItemStack ret = storage[i];
        storage[i] = null;
        return ret;
    }

    @Override
    public void setInventorySlotContents(int i, ItemStack itemstack) {
        storage[i] = itemstack;
    }

    @Override
    public String getInvName() {
        return invName;
    }

    @Override
    public int getInventoryStackLimit() {
        return slotLimit;
    }

    @Override
    public void onInventoryChanged() {
    }

    @Override
    public boolean isUseableByPlayer(EntityPlayer entityplayer) {
        return false;
    }

    @Override
    public void openChest() {
    }

    @Override
    public void closeChest() {
    }

    public void load(NBTTagCompound tag) {
        load(tag, "");
    }

    public void load(NBTTagCompound tag, String prefix) {
        if (tag == null)
            return;
        
        NBTTagList nbttaglist = tag.getTagList(prefix + "items");

        for (int i = 0; i < nbttaglist.tagCount(); i++) {
            NBTTagCompound nbttagcompound2 = (NBTTagCompound) nbttaglist.tagAt(i);
            int index = nbttagcompound2.getInteger("index");
            if (index < storage.length)
                storage[index] = ItemStack.loadItemStackFromNBT(nbttagcompound2);
        }
        onInventoryChanged();
    }

    public void save(NBTTagCompound tag) {
        save(tag, "");
    }

    public void save(NBTTagCompound tag, String prefix) {
        if (tag == null)
            return;
        
        NBTTagList itemList = new NBTTagList();
        for (int i = 0; i < storage.length; i++) {
            if (storage[i] != null && storage[i].stackSize > 0) {
                NBTTagCompound tag2 = new NBTTagCompound();
                tag2.setInteger("index", i);
                storage[i].writeToNBT(tag2);
                itemList.appendTag(tag2);
            }
        }
        tag.setTag(prefix + "items", itemList);
        tag.setInteger(prefix + "itemsCount", storage.length);
    }

    public void dropContents(World worldObj, int posX, int posY, int posZ) {
        if (BasicUtils.isServer(worldObj)) {
            for (int i = 0; i < storage.length; i++) {
                while (storage[i] != null) {
                    ItemStack todrop = decrStackSize(i, storage[i].getMaxStackSize());
                    BasicUtils.dropItem(worldObj, posX, posY, posZ, todrop);
                }
            }
            onInventoryChanged();
        }
    }

    @Override
    public ItemStack getStackInSlotOnClosing(int i) {
        if (this.storage[i] == null)
            return null;

        ItemStack stackToTake = this.storage[i];
        this.storage[i] = null;
        onInventoryChanged();
        return stackToTake;
    }

    //TODO remove?
    private int tryAddToSlot(int i, ItemStack stack) {
        ItemStack slot = storage[i];
        if (slot == null) {
            storage[i] = stack.copy();
            return stack.stackSize;
        }
        if (BasicUtils.areStacksTheSame(slot, stack)) {
            slot.stackSize += stack.stackSize;
            if (slot.stackSize > 127) {
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

    //TODO remove?
    public int addCompressed(ItemStack stack) {
        if (stack == null)
            return 0;
        stack = stack.copy();
        for (int i = 0; i < storage.length; i++) {
            if (stack.stackSize <= 0) {
                break;
            }
            if (storage[i] == null)
                continue; // Skip Empty Slots on first attempt.
            int added = tryAddToSlot(i, stack);
            stack.stackSize -= added;
        }
        for (int i = 0; i < storage.length; i++) {
            if (stack.stackSize <= 0) {
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
    public boolean isItemValidForSlot(int i, ItemStack itemstack) {
        return false;
    }

    public ItemStack[] getContents() {
        return storage;
    }
}
