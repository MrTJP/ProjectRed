/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.compatibility.mfr

import mrtjp.core.inventory.{IInvWrapperRegister, InvWrapper}
import mrtjp.core.item.ItemKey
import mrtjp.projectred.compatibility.IPRPlugin
import mrtjp.projectred.core.Configurator
import net.minecraft.inventory.IInventory
import powercrystals.minefactoryreloaded.api.IDeepStorageUnit

object PluginMFRDeepStorage extends IPRPlugin
{
    override def getModIDs = Array("ProjRed|Transportation")

    override def isEnabled = Configurator.compat_MFRDeepStorage

    override def preInit(){}

    override def init(){}

    override def postInit()
    {
        InvWrapper.register(DSUInvWrapperRegister)
    }

    override def desc() = "MFR: Deep Storage Unit pipe interactions"
}

object DSUInvWrapperRegister extends IInvWrapperRegister
{
    override def wrapperID = "mfr_deepstorage"
    override def matches(inv:IInventory) = inv.isInstanceOf[IDeepStorageUnit]
    override def create(inv:IInventory) = new DeepStorageInvWrapper(inv)
}

class DeepStorageInvWrapper(inv:IInventory) extends InvWrapper(inv)
{
    def getDS = inv.asInstanceOf[IDeepStorageUnit]

    override def getSpaceForItem(item:ItemKey) =
    {
        val stack = getDS.getStoredItemType
        if (stack != null && ItemKey.get(stack) == item) getDS.getMaxStoredCount-stack.stackSize
        else if (stack == null) getDS.getMaxStoredCount
        else 0
    }

    override def hasSpaceForItem(item:ItemKey) =
    {
        val stack = getDS.getStoredItemType
        if (stack != null && ItemKey.get(stack) == item) getDS.getMaxStoredCount-stack.stackSize > 0
        else if (stack == null) getDS.getMaxStoredCount > 0
        else false
    }

    override def getItemCount(item:ItemKey) =
    {
        val stack = getDS.getStoredItemType
        if (stack != null && ItemKey.get(stack) == item) stack.stackSize
        else 0
    }

    override def hasItem(item:ItemKey) = ItemKey.getOrNull(getDS.getStoredItemType) == item

    override def injectItem(item:ItemKey, toAdd:Int):Int =
    {
        val stack = getDS.getStoredItemType
        if (stack != null && ItemKey.get(stack) == item)
        {
            val spaceLeft = getDS.getMaxStoredCount-stack.stackSize
            val maxToAdd = math.min(toAdd, spaceLeft)
            getDS.setStoredItemCount(stack.stackSize+maxToAdd)
            return maxToAdd
        }
        if (stack == null)
        {
            val spaceLeft = getDS.getMaxStoredCount
            val maxToAdd = math.min(toAdd, spaceLeft)
            getDS.setStoredItemType(item.makeStack(0), maxToAdd)
            return maxToAdd
        }
        0
    }

    override def extractItem(item:ItemKey, toExtract:Int) =
    {
        val stack = getDS.getStoredItemType
        if (stack != null && ItemKey.get(stack) == item)
        {
            val maxToExtract = math.min(stack.stackSize, toExtract)
            getDS.setStoredItemCount(stack.stackSize-maxToExtract)
            maxToExtract
        }
        else 0
    }

    override def getAllItemStacks =
    {
        val stack = getDS.getStoredItemType
        if (stack != null) Map(ItemKey.get(stack) -> stack.stackSize)
        else Map.empty
    }
}