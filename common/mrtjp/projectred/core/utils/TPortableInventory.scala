package mrtjp.projectred.core.utils

import mrtjp.projectred.core.inventory.SimpleInventory
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.IInventory
import net.minecraft.item.ItemStack

trait TPortableInventory extends IInventory
{
    val inv = createInv

    def getSizeInventory = inv.getSizeInventory
    def getStackInSlot(i:Int) = inv.getStackInSlot(i)
    def decrStackSize(i:Int, j:Int) = inv.decrStackSize(i, j)
    def getStackInSlotOnClosing(i:Int) = inv.getStackInSlotOnClosing(i)
    def setInventorySlotContents(i:Int, itemstack:ItemStack) = inv.setInventorySlotContents(i, itemstack)
    def getInvName:String = inv.getInvName
    def isInvNameLocalized = inv.isInvNameLocalized
    def getInventoryStackLimit = inv.getInventoryStackLimit
    def onInventoryChanged() = inv.onInventoryChanged
    def isUseableByPlayer(entityplayer:EntityPlayer) = inv.isUseableByPlayer(entityplayer)
    def openChest() = inv.openChest
    def closeChest() = inv.closeChest
    def isItemValidForSlot(i:Int, itemstack:ItemStack) = inv.isItemValidForSlot(i, itemstack)

    def createInv:SimpleInventory
}
