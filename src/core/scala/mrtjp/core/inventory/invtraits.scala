/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.inventory

import mrtjp.core.world.WorldLib
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.inventory.IInventory
import net.minecraft.item.ItemStack
import net.minecraft.nbt.{CompoundNBT, ListNBT}
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World

trait TInventory extends IInventory
{
    protected val storage:Array[ItemStack]

    override def getContainerSize:Int = storage.length

    override def isEmpty:Boolean = storage.forall(_.isEmpty)

    override def stillValid(player:PlayerEntity):Boolean = true
    override def canPlaceItem(slot:Int, item:ItemStack):Boolean = true

    override def startOpen(player:PlayerEntity):Unit = {}
    override def stopOpen(player:PlayerEntity):Unit = {}

    override def getItem(slot:Int):ItemStack = storage(slot)

    override def setItem(slot:Int, item:ItemStack):Unit = {
        storage(slot) = item
        setChanged()
    }

    override def removeItemNoUpdate(slot:Int):ItemStack = {
        val stack = storage(slot)
        if (!stack.isEmpty) {
            storage(slot) = ItemStack.EMPTY
            setChanged()
        }
        stack
    }

    override def removeItem(slot:Int, count:Int):ItemStack = {
        val stack = storage(slot)
        if (stack.isEmpty) return ItemStack.EMPTY

        if (stack.getCount > count) {
            val out = stack.split(count)
            setChanged()
            out
        } else {
            val out = stack
            storage(slot) = ItemStack.EMPTY
            setChanged()
            out
        }
    }

    override def clearContent():Unit = {
        for (i <- storage.indices)
            storage(i) = ItemStack.EMPTY
    }

    def nbtSaveName:String

    def loadInv(tag:CompoundNBT):Unit = loadInv(tag, nbtSaveName)
    def loadInv(tag:CompoundNBT, prefix:String):Unit = {
        val tag1 = tag.getList(prefix+"items", 10)
        for (i <- 0 until tag1.size()) {
            val tag2 = tag1.getCompound(i)

            val index = tag2.getInt("index")
            if (storage.isDefinedAt(index))
                storage(index) = ItemStack.of(tag2)
        }
    }

    def saveInv(tag:CompoundNBT):Unit = saveInv(tag, nbtSaveName)
    def saveInv(tag:CompoundNBT, prefix:String):Unit = {
        val itemList = new ListNBT()
        for (i <- storage.indices) if (!storage(i).isEmpty && storage(i).getCount > 0) {
            val tag2 = new CompoundNBT
            tag2.putInt("index", i)
            storage(i).save(tag2)
            itemList.add(tag2)
        }

        tag.put(prefix+"items", itemList)
        tag.putInt(prefix+"itemsCount", storage.length)
    }

    def dropInvContents(w:World, pos:BlockPos):Unit = {
        for (i <- storage) if (!i.isEmpty) WorldLib.dropItem(w, pos, i)
        for (i <- storage.indices) storage(i) = ItemStack.EMPTY
        setChanged()
    }
}
