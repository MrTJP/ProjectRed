package mrtjp.projectred.core.inventory;

import mrtjp.projectred.core.BasicUtils;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.IInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.world.World;

public class SimpleInventory implements IInventory
{
    private final String invName;
    private ItemStack[] storage;
    private final int slotLimit;

    public SimpleInventory(int size, String name, int stackLimit)
    {
        storage = new ItemStack[size];
        invName = name;
        slotLimit = stackLimit;
    }

    @Override
    public int getSizeInventory()
    {
        return storage.length;
    }

    @Override
    public ItemStack getStackInSlot(int i)
    {
        return storage[i];
    }

    @Override
    public ItemStack decrStackSize(int i, int j)
    {
        if (storage[i] == null)
            return null;
        if (storage[i].stackSize > j)
        {
            ItemStack ret = storage[i].splitStack(j);
            return ret;
        }
        ItemStack ret = storage[i];
        storage[i] = null;
        return ret;
    }

    @Override
    public void setInventorySlotContents(int i, ItemStack itemstack)
    {
        storage[i] = itemstack;
    }

    @Override
    public String getInvName()
    {
        return invName;
    }

    @Override
    public int getInventoryStackLimit()
    {
        return slotLimit;
    }

    @Override
    public void onInventoryChanged()
    {
    }

    @Override
    public boolean isUseableByPlayer(EntityPlayer entityplayer)
    {
        return true;
    }

    @Override
    public void openChest()
    {
    }

    @Override
    public void closeChest()
    {
    }

    public void load(NBTTagCompound tag)
    {
        load(tag, "");
    }

    public void load(NBTTagCompound tag, String prefix)
    {
        if (tag == null)
            return;

        NBTTagList tag1 = tag.getTagList(prefix + "items");

        for (int i = 0; i < tag1.tagCount(); i++)
        {
            NBTTagCompound tag2 = (NBTTagCompound) tag1.tagAt(i);
            int index = tag2.getInteger("index");
            if (index < storage.length)
                storage[index] = ItemStack.loadItemStackFromNBT(tag2);
        }
        onInventoryChanged();
    }

    public void save(NBTTagCompound tag)
    {
        save(tag, "");
    }

    public void save(NBTTagCompound tag, String prefix)
    {
        if (tag == null)
            return;

        NBTTagList itemList = new NBTTagList();
        for (int i = 0; i < storage.length; i++)
            if (storage[i] != null && storage[i].stackSize > 0)
            {
                NBTTagCompound tag2 = new NBTTagCompound();
                tag2.setInteger("index", i);
                storage[i].writeToNBT(tag2);
                itemList.appendTag(tag2);
            }
        tag.setTag(prefix + "items", itemList);
        tag.setInteger(prefix + "itemsCount", storage.length);
    }

    public void dropContents(World worldObj, int posX, int posY, int posZ)
    {
        if (BasicUtils.isServer(worldObj))
        {
            for (int i = 0; i < storage.length; i++)
                while (storage[i] != null)
                {
                    ItemStack todrop = decrStackSize(i, storage[i].getMaxStackSize());
                    BasicUtils.dropItem(worldObj, posX, posY, posZ, todrop);
                }
            onInventoryChanged();
        }
    }

    @Override
    public ItemStack getStackInSlotOnClosing(int i)
    {
        if (this.storage[i] == null)
            return null;

        ItemStack stackToTake = this.storage[i];
        this.storage[i] = null;
        onInventoryChanged();
        return stackToTake;
    }

    @Override
    public boolean isInvNameLocalized()
    {
        return true;
    }

    @Override
    public boolean isItemValidForSlot(int i, ItemStack itemstack)
    {
        return true;
    }

    public ItemStack[] getContents()
    {
        return storage;
    }
}
