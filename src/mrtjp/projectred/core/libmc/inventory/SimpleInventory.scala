package mrtjp.projectred.core.libmc.inventory

import net.minecraft.inventory.IInventory
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.nbt.{NBTTagList, NBTTagCompound}
import net.minecraft.world.World
import mrtjp.projectred.core.libmc.PRLib

class SimpleInventory(size:Int, name:String, stackLimit:Int) extends IInventory
{
    def this(size:Int, lim:Int) = this(size, "", lim)
    def this(size:Int, name:String) = this(size, name, 64)
    def this(size:Int) = this(size, 64)

    private val storage = new Array[ItemStack](size)

    override def getSizeInventory = storage.length
    override def getInventoryStackLimit = stackLimit
    override def hasCustomInventoryName = true
    override def getInventoryName = name
    override def isUseableByPlayer(var1:EntityPlayer) = true

    override def openInventory(){}
    override def closeInventory(){}
    override def markDirty(){}

    override def isItemValidForSlot(var1:Int, var2:ItemStack) = true

    override def getStackInSlot(var1:Int) = storage(var1)
    override def getStackInSlotOnClosing(i:Int):ItemStack =
    {
        if (storage(i) == null) return null
        val stackToTake = storage(i)
        storage(i) = null

        markDirty()
        stackToTake
    }

    override def setInventorySlotContents(var1:Int, var2:ItemStack){storage(var1) = var2}
    override def decrStackSize(i:Int, j:Int):ItemStack =
    {
        if (storage(i) == null) return null
        if (storage(i).stackSize > j) return storage(i).splitStack(j)

        val ret = storage(i)
        storage(i) = null
        ret
    }

    def load(tag:NBTTagCompound){load(tag, name)}
    def load(tag:NBTTagCompound, prefix:String)
    {
        val tag1 = tag.getTagList(prefix+"items", 10 /*COMPOUND*/)
        for (i <- 0 until tag1.tagCount())
        {
            val tag2 = tag1.getCompoundTagAt(i)

            val index = tag2.getInteger("index")
            if (storage.isDefinedAt(index)) storage(index) = ItemStack.loadItemStackFromNBT(tag2)
        }
    }

    def save(tag:NBTTagCompound){save(tag, name)}
    def save(tag:NBTTagCompound, prefix:String)
    {
        val itemList = new NBTTagList
        for (i <- 0 until storage.length) if (storage(i) != null && storage(i).stackSize > 0)
        {
            val tag2 = new NBTTagCompound
            tag2.setInteger("index", i)
            storage(i).writeToNBT(tag2)
            itemList.appendTag(tag2)
        }

        tag.setTag(prefix+"items", itemList)
        tag.setInteger(prefix+"itemsCount", storage.length)
    }

    def dropContents(w:World, x:Int, y:Int, z:Int)
    {
        if (w.isRemote) return

        for (i <- storage) if (i != null)
            PRLib.dropItem(w, x, y, z, i)
        for (i <- 0 until storage.length) storage(i) = null
        markDirty()
    }
}
