/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.inventory

import net.minecraft.item.ItemStack

class SimpleInventory(size:Int, name:String, stackLimit:Int) extends TInventory
{
    def this(size:Int, lim:Int) = this(size, "", lim)
    def this(size:Int, name:String) = this(size, name, 64)
    def this(size:Int) = this(size, 64)

    override protected val storage  = Array.fill(size)(ItemStack.EMPTY)//new Array[ItemStack](size)

    override def getMaxStackSize:Int = stackLimit

    override def nbtSaveName = name

    override def setChanged():Unit = {}
}

class ArrayWrapInventory(override protected val storage:Array[ItemStack], name:String, stackLimit:Int) extends TInventory
{
    override def getMaxStackSize:Int = stackLimit

    override def setChanged():Unit = {}

    override def nbtSaveName = name
}