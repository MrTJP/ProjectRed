/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.inventory

import mrtjp.core.item.{ItemEquality, ItemKey, ItemKeyStack}
import net.minecraft.inventory.{IInventory, ISidedInventory}
import net.minecraft.item.ItemStack
import net.minecraft.util.Direction
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World
import net.minecraftforge.common.util.LazyOptional
import net.minecraftforge.items.CapabilityItemHandler._
import net.minecraftforge.items.IItemHandler
import net.minecraftforge.items.wrapper.EmptyHandler

object InvWrapper
{
    var wrappers = Seq[IInvWrapperRegister]()

    def register(w:IInvWrapperRegister)
    {
        for (wr <- wrappers) if (wr.wrapperID == w.wrapperID) return
        wrappers :+= w
    }

    @deprecated
    def wrap(inv:IInventory):InvWrapper =
    {
        for (w <- wrappers) if (w.matches(inv)) return w.create(inv)
        new VanillaWrapper(inv, false)
    }

    //Used for wrapping inventory tiles
    def wrap(world:World, pos:BlockPos, side:Direction):InvWrapper =
    {
        val tile = world.getBlockEntity(pos)
        if (tile == null) return null

        def wrap(opt:LazyOptional[IItemHandler]):InvWrapper = {
            if (opt.isPresent) {
                val handler = opt.orElse(EmptyHandler.INSTANCE)
                if (handler.getSlots > 0)
                    return new CapWrapper(handler)
            }
            null
        }

        var ret = wrap(tile.getCapability(ITEM_HANDLER_CAPABILITY, side))
        if (ret == null) {
            ret = wrap(tile.getCapability(ITEM_HANDLER_CAPABILITY, null))
        }

        ret
//        tile match {
//            case inv:IInventory =>
//                val wr = new VanillaWrapper(inv, false)
//                if (side != null)
//                    wr.setSlotsFromSide(side.getIndex)
//                else
//                    wr.setSlotsAll()
//                wr
//            case _ => null
//        }
    }

    //Used for wrapping raw inventories for help with internal inventory manipulation
    def wrapInternal(inv:IInventory):VanillaWrapper = wrapInternal(inv, 0 until inv.getContainerSize)

    def wrapInternal(inv:IInventory, slots:Range):VanillaWrapper =
    {
        val wr = new VanillaWrapper(inv, true)
        wr.setSlotsFromRange(slots)
        wr
    }

    def areItemsStackable(stack1:ItemStack, stack2:ItemStack):Boolean =
    {
        stack1.isEmpty || stack2.isEmpty || areItemsSame(stack1, stack2) && stack1.isStackable && stack2.isStackable
    }

    def areItemsSame(stack1:ItemStack, stack2:ItemStack):Boolean =
    {
        if (stack1.isEmpty || stack2.isEmpty) return stack1 == stack2
        stack1.getItem == stack2.getItem && ItemStack.tagMatches(stack2, stack1)
    }

//    def getInventory(world:World, pos:BlockPos):IInventory =
//        world.getTileEntity(pos) match {
//            case chest:TileEntityChest =>
//                var lower:TileEntityChest = null
//                var upper:TileEntityChest = null
//                if (chest.adjacentChestXNeg != null) {
//                    upper = chest.adjacentChestXNeg
//                    lower = chest
//                }
//                else if (chest.adjacentChestXPos != null) {
//                    upper = chest
//                    lower = chest.adjacentChestXPos
//                }
//                else if (chest.adjacentChestZNeg != null) {
//                    upper = chest.adjacentChestZNeg
//                    lower = chest
//                }
//                else if (chest.adjacentChestZPos != null) {
//                    upper = chest
//                    lower = chest.adjacentChestZPos
//                }
//                if (lower != null && upper != null) new HashableLargeChest("Large Chest", upper, lower)
//                else chest
//            case inv:IInventory => inv
//            case _ => null
//        }
}

//class HashableLargeChest(name:String, val inv1:TileEntityChest, val inv2:TileEntityChest) extends InventoryLargeChest(name, inv1, inv2)
//{
//    override def hashCode = inv1.hashCode^inv2.hashCode
//
//    override def equals(other:Any) = other match
//    {
//        case that:HashableLargeChest => inv1 == that.inv1 && inv2 == that.inv2
//        case _ => false
//    }
//}

trait IInvWrapperRegister
{
    /**
     * Unique ID for each type of wrapper.
     */
    def wrapperID:String

    /**
     * Returns true if this wrapper should be used for the given inventory.
     */
    def matches(inv:IInventory):Boolean

    /**
     * Returns a new instance of this wrapper.
     */
    def create(inv:IInventory):InvWrapper
}

abstract class InvWrapper
{
    protected var hidePerSlot = false
    protected var hidePerType = false
    protected var eq = new ItemEquality

    def setMatchOptions(nbt:Boolean, tags:Boolean) =
    {
        eq.matchNBT = nbt
        eq.matchTags = tags
        this
    }

    def setMatchOptions(other:ItemEquality)
    {
        setMatchOptions(other.matchNBT, other.matchTags)
    }

    def setHidePerSlot(flag:Boolean) =
    {
        hidePerSlot = flag
        if (flag) hidePerType = false
        this
    }

    def setHidePerType(flag:Boolean) =
    {
        hidePerType = flag
        if (flag) hidePerSlot = false
        this
    }

    /**
     * Get a count for how many items of this type can be shoved into the
     * inventory.
     *
     * @param item The item to count free space for. Not manipulated in any way.
     * @return The number of those items this inventory can still take.
     */
    def getSpaceForItem(item:ItemKey):Int

    /**
     * Check if at least one of this item can fit. Failfast for
     * getSpaceForItem
     * @param item The item to count free space for. Not manipulated in any way.
     * @return True if one of these items can fit
     */
    def hasSpaceForItem(item:ItemKey):Boolean

    /**
     * Counts how many of those items this inventory contains.
     *
     * @param item The item to count. Not manipulated in any way.
     * @return The number of those items this inventory contains.
     */
    def getItemCount(item:ItemKey):Int

    /**
     * Returns if the given item is in the inventory somewhere. Failfast of
     * getItemCount
     *
     * @param item the item. Not manipulated in any way.
     * @return
     */
    def hasItem(item:ItemKey):Boolean

    /**
     * Inject the ItemStack into the inventory, starting with merging, then to
     * empty slots.
     *
     * @param item The item to try and merge. Not manipulated in any way.
     * @param toAdd Amount to try to add.
     * @return The number of items that were merged in, between 0 and toAdd.
     */
    def injectItem(item:ItemKey, toAdd:Int):Int

    /**
     * Extract the item a specified number of times.
     *
     * @param item Item to extract from inventory. Not manipulated in any way.
     * @param toExtract Amount to try to extract.
     * @return Amount extracted, between 0 and toExtract.
     */
    def extractItem(item:ItemKey, toExtract:Int):Int

    /**
     * Return an ordered map of all available [ItemStack, Amount] in the
     * inventory. The actual inventory is not manipulated.
     *
     * @return
     */
    def getAllItemStacks:Map[ItemKey, Int]
}

/**
  * TODO make these wrapper methods take into account hidePerType and hidePerSlot
  */
class CapWrapper(cap:IItemHandler) extends InvWrapper
{
    override def getSpaceForItem(item:ItemKey) =
    {
        var space = 0
        for (s <- 0 until cap.getSlots) {
            val stack = item.makeStack(math.min(item.getMaxStackSize, cap.getSlotLimit(s)))
            val remaining = cap.insertItem(s, stack, true)
            val inserted = stack.getCount-remaining.getCount
            space += inserted
        }
        space
    }

    override def hasSpaceForItem(item:ItemKey):Boolean =
    {
        for (s <- 0 until cap.getSlots) {
            val stack = item.makeStack(item.getMaxStackSize)
            val remaining = cap.insertItem(s, stack, true)
            val inserted = stack.getCount-remaining.getCount
            if (inserted > 0)
                return true
        }
        false
    }

    override def getItemCount(item:ItemKey) =
    {
        var count = 0
        for (s <- 0 until cap.getSlots) {
            val slotItem = ItemKeyStack.get(cap.getStackInSlot(s))
            if (!slotItem.isEmpty && eq.matches(item, slotItem.key))
                count += slotItem.stackSize
        }
        count
    }

    override def hasItem(item:ItemKey):Boolean =
    {
        for (s <- 0 until cap.getSlots) {
            val slotItem = ItemKeyStack.get(cap.getStackInSlot(s))
            if (!slotItem.isEmpty && eq.matches(item, slotItem.key))
                return true
        }
        false
    }

    override def injectItem(item:ItemKey, toAdd:Int):Int =
    {
        var itemsLeft = toAdd
        for (s <- 0 until cap.getSlots) {
            var amountToAdd = itemsLeft
            amountToAdd = math.min(amountToAdd, item.getMaxStackSize)
            amountToAdd = math.min(amountToAdd, cap.getSlotLimit(s))

            val stackToAdd = item.makeStack(amountToAdd)
            val leftoverStack = cap.insertItem(s, stackToAdd, false)

            val amountInserted = stackToAdd.getCount-leftoverStack.getCount

            itemsLeft -= amountInserted
            if (itemsLeft <= 0)
                return toAdd
        }
        toAdd-itemsLeft
    }

    override def extractItem(item:ItemKey, toExtract:Int):Int =
    {
        var itemsLeft = toExtract
        var didMatch = false

        for (s <- 0 until cap.getSlots) {
            val stackInSlot = cap.getStackInSlot(s)
            val keyInSlot = ItemKey.get(stackInSlot)
            if (eq.matches(item, keyInSlot)) {
                var amountAvailable = stackInSlot.getCount
                if (hidePerSlot)
                    amountAvailable -= 1
                else if (hidePerType && !didMatch)
                    amountAvailable -= 1

                val amountToExtract = math.min(itemsLeft, amountAvailable)
                val stack = cap.extractItem(s, amountToExtract, false)

                itemsLeft -= stack.getCount
                if (itemsLeft <= 0)
                    return toExtract

                didMatch = true
            }
        }
        toExtract-itemsLeft
    }

    override def getAllItemStacks =
    {
        var items = Map[ItemKey, Int]()
        for (s <- 0 until cap.getSlots) {
            val inSlot = cap.getStackInSlot(s)
            if (!inSlot.isEmpty) {
                val key = ItemKey.get(inSlot)
                val stackSize = inSlot.getCount-(if (hidePerSlot) 1 else 0)
                val currentSize = items.getOrElse(key, 0)

                if (!items.keySet.contains(key)) items += key -> (stackSize-(if (hidePerType) 1 else 0))
                else items += key -> (currentSize+stackSize)
            }
        }
        items
    }
}

class VanillaWrapper(inv:IInventory, internalMode:Boolean) extends InvWrapper
{
    protected val sidedInv = inv match
    {
        case inv2:ISidedInventory => inv2
        case _ => null
    }
    protected var side:Direction = null

    var slots:Seq[Int] = 0 until inv.getContainerSize

    def setSlotsFromSide(s:Int) =
    {
        if (sidedInv != null)
        {
            side = Direction.values()(s)
            slots = sidedInv.getSlotsForFace(side)
        }
        else setSlotsAll()
        this
    }

    def setSlotsFromRange(r:Range) =
    {
        side = null
        slots = r
        this
    }

    def setSlotsAll() =
    {
        side = null
        slots = (0 until inv.getContainerSize)
        this
    }

    override def getSpaceForItem(item:ItemKey):Int =
    {
        var space = 0
        val item2 = item.testStack
        val slotStackLimit = math.min(inv.getMaxStackSize, item.getMaxStackSize)
        for (slot <- slots)
        {
            val s = inv.getItem(slot)
            if (canInsertItem(slot, item2))
            {
                if (s.isEmpty) space += slotStackLimit
                else if (InvWrapper.areItemsStackable(s, item2)) space += slotStackLimit-s.getCount
            }
        }
        space
    }

    override def hasSpaceForItem(item:ItemKey):Boolean =
    {
        val item2 = item.testStack
        val slotStackLimit = math.min(inv.getMaxStackSize, item2.getMaxStackSize)
        for (slot <- slots)
        {
            val s = inv.getItem(slot)
            if (canInsertItem(slot, item2))
            {
                if (s.isEmpty) return true
                else if (InvWrapper.areItemsStackable(s, item2) && slotStackLimit-s.getCount > 0) return true
            }
        }
        false
    }

    override def getItemCount(item:ItemKey) =
    {
        var count = 0

        var first = true

        for (slot <- slots)
        {
            val inSlot = inv.getItem(slot)
            if (!inSlot.isEmpty && eq.matches(item, ItemKey.get(inSlot)))
            {
                val toAdd = inSlot.getCount-(if (hidePerSlot || hidePerType && first) 1 else 0)
                first = false
                count += toAdd
            }
        }
        count
    }

    override def hasItem(item:ItemKey):Boolean =
    {
        for (slot <- slots)
        {
            val inSlot = inv.getItem(slot)
            if (!inSlot.isEmpty && eq.matches(item, ItemKey.get(inSlot))) return true
        }

        false
    }

    override def injectItem(item:ItemKey, toAdd:Int):Int =
    {
        var itemsLeft = toAdd
        val slotStackLimit = math.min(inv.getMaxStackSize, item.getMaxStackSize)

        for (pass <- Seq(0, 1)) for (slot <- slots) if (canInsertItem(slot, item.testStack))
        {
            val inSlot = inv.getItem(slot)

            if (!inSlot.isEmpty && InvWrapper.areItemsStackable(item.testStack, inSlot))
            {
                val fit = math.min(slotStackLimit-inSlot.getCount, itemsLeft)
                inSlot.grow(fit)
                itemsLeft -= fit
                inv.setItem(slot, inSlot)
            }
            else if (pass == 1 && inSlot.isEmpty)
            {
                val toInsert = item.makeStack(math.min(inv.getMaxStackSize, itemsLeft))
                itemsLeft -= toInsert.getCount
                inv.setItem(slot, toInsert)
            }

            if (itemsLeft == 0) return toAdd
        }

        toAdd-itemsLeft
    }

    override def extractItem(item:ItemKey, toExtract:Int):Int =
    {
        if (toExtract <= 0) return 0
        var left = toExtract
        var first = true
        for (slot <- slots) if (canExtractItem(slot, item.testStack))
        {
            val inSlot = inv.getItem(slot)
            if (!inSlot.isEmpty && eq.matches(item, ItemKey.get(inSlot))) //TODO extraction shouldnt rely on eq matches..?
            {
                left -= inv.removeItem(slot, math.min(left, inSlot.getCount-(if (hidePerSlot || hidePerType&&first) 1 else 0))).getCount
                first = false
            }
            if (left <= 0) return toExtract
        }
        toExtract - left
    }

    override def getAllItemStacks =
    {
        var items = Map[ItemKey, Int]()
        for (slot <- slots)
        {
            val inSlot = inv.getItem(slot)
            if (!inSlot.isEmpty)
            {
                val key = ItemKey.get(inSlot)
                val stackSize = inSlot.getCount-(if (hidePerSlot) 1 else 0)
                val currentSize = items.getOrElse(key, 0)

                if (!items.keySet.contains(key)) items += key -> (stackSize-(if (hidePerType) 1 else 0))
                else items += key -> (currentSize+stackSize)
            }
        }
        items
    }

    protected def canInsertItem(slot:Int, item:ItemStack):Boolean =
    {
        if (internalMode) return true
        if (side == null) inv.canPlaceItem(slot, item) else sidedInv.canPlaceItemThroughFace(slot, item, side)
    }

    protected def canExtractItem(slot:Int, item:ItemStack):Boolean =
    {
        if (internalMode) return true
        if (side == null) inv.canPlaceItem(slot, item) else sidedInv.canTakeItemThroughFace(slot, item, side)
    }
}
