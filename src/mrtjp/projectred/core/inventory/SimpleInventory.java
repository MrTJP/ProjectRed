package mrtjp.projectred.core.inventory;

import mrtjp.projectred.core.libmc.BasicUtils;
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
    public String getInventoryName()
    {
        return invName;
    }

    @Override
    public boolean hasCustomInventoryName()
    {
        return true;
    }

    @Override
    public int getInventoryStackLimit()
    {
        return slotLimit;
    }

    @Override
    public void markDirty()
    {
    }

    @Override
    public boolean isUseableByPlayer(EntityPlayer entityplayer)
    {
        return true;
    }

    @Override
    public void openInventory()
    {
    }

    @Override
    public void closeInventory()
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

        NBTTagList tag1 = tag.getTagList(prefix+"items", 0);

        for (int i = 0; i < tag1.tagCount(); i++)
        {
            NBTTagCompound tag2 = tag1.getCompoundTagAt(i);
            int index = tag2.getInteger("index");
            if (index < storage.length)
                storage[index] = ItemStack.loadItemStackFromNBT(tag2);
        }
        markDirty();
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
        tag.setTag(prefix+"items", itemList);
        tag.setInteger(prefix+"itemsCount", storage.length);
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
            markDirty();
        }
    }

    @Override
    public ItemStack getStackInSlotOnClosing(int i)
    {
        if (this.storage[i] == null)
            return null;

        ItemStack stackToTake = this.storage[i];
        this.storage[i] = null;
        markDirty();
        return stackToTake;
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
