/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.gui

import mrtjp.core.inventory.InvWrapper
import net.minecraft.client.Minecraft
import net.minecraft.entity.player.{PlayerEntity, PlayerInventory}
import net.minecraft.inventory._
import net.minecraft.inventory.container._
import net.minecraft.item.ItemStack
import net.minecraftforge.api.distmarker.{Dist, OnlyIn}

class NodeContainer(containerType:ContainerType[_], windowId:Int) extends Container(containerType, windowId)
{
    var startWatchDelegate = {(p:PlayerEntity) => }
    var stopWatchDelegate = {(p:PlayerEntity) => }
    var slotChangeDelegate = {(slot:Int) => }

//    def slots:MBuffer[TSlot3] = inventorySlots.asInstanceOf[JList[TSlot3]].asScala

    override def stillValid(player:PlayerEntity) = true

    override def canDragTo(slot:Slot) = slot match
    {
        case s:TSlot3 => !s.phantomSlot
        case _ => super.canDragTo(slot)
    }

    override def addSlot(slot:Slot) =
    {
//        if (!slot.isInstanceOf[TSlot3])
//            throw new IllegalArgumentException("NodeContainers can only except slots of type Slot3")
        super.addSlot(slot)

//        slot.asInstanceOf[TSlot3].slotChangeDelegate2 =
//                {() => slotChangeDelegate(slot.slotNumber)}
//        slot
    }

    @OnlyIn(Dist.CLIENT)
    def addPlayerInv(x:Int, y:Int):Unit = addPlayerInv(Minecraft.getInstance.player.inventory, x, y)

    def addPlayerInv(playerInv:PlayerInventory, x:Int, y:Int):Unit = {
        addPlayerInv(playerInv, x, y, (inv, i, x, y) => new Slot(inv, i, x, y))
    }

    def addPlayerInv(playerInv:PlayerInventory, x:Int, y:Int, slotFactory:(PlayerInventory,  Int, Int, Int) => Slot):Unit = {
        var next = 0
        def up() = {next+=1;next-1}

        for ((x, y) <- GuiLib.createSlotGrid(x, y+58, 9, 1, 0, 0))
            addSlot(slotFactory(playerInv, up(), x, y)) //hotbar

        for ((x, y) <- GuiLib.createSlotGrid(x, y, 9, 3, 0, 0))
            addSlot(slotFactory(playerInv, up(), x, y)) //slots
    }

    override def addSlotListener(listener:IContainerListener)
    {
        super.addSlotListener(listener)
        listener match {
            case p:PlayerEntity if !p.level.isClientSide =>
                startWatchDelegate(p)
            case _ =>
        }
    }


    override def removeSlotListener(listener:IContainerListener)
    {
        super.removeSlotListener(listener)
        listener match {
            case p:PlayerEntity if !p.level.isClientSide =>
                stopWatchDelegate(p)
            case _ =>
        }
    }

    override def removed(p:PlayerEntity)
    {
        super.removed(p)
        if (!p.level.isClientSide)
            stopWatchDelegate(p)
    }

    /**
      * Called when a slot in this container is clicked, or any other event that occurs
      * that results in a item stack moving such as hotbar swapping w/ numbers 0-8.
      * @param id The slot ID
      * @param dragType Changes meaning depending on clickType:
      *                 PICKUP: The mouse button 0 = left click, 1 = right click
      *                 QUICK_MOVE: The mouse button
      *                 SWAP: The keyboard key that was pressed 0 to 8
      *                 QUICK_CRAFT: Some type of mask. Look at super method.
      *                 PICKUP_ALL: The mouse button
      *
      * @param clickType The type of event that is being performed:
      *                  PICKUP: the stack is trying to be picked up to or dropped from in the player's cursor.
      *                          If the ID is -999, the player has clicked outside the window and wants to drop
      *                          the item in-world.
      *                  QUICK_MOVE: the stack was shift-clicked.
      *                  SWAP: a number was pressed to swap a hotbar item.
      *                  CLONE: Player has cloned item while in creative mode.
      *                  THROW: Hotkey to throw item into the world was pressed.
      *                  QUICK_CRAFT: The stack in-hand was dragged across several slots to split them up, or just simply
      *                               right-clicked to deposit one of the items. Handled in super method. Too convoluted
      *                               to understand, as Forge's documentation sucks. Best to just let MC handle this...
      *                  PICKUP_ALL: When a slot with an item is double-clicked to pick up all matching items in the container.
      *
      *
      *
      * @param player The player that has this container open.
      * @return
      */
    override def clicked(id:Int, dragType:Int, clickType:ClickType, player:PlayerEntity):ItemStack =
    {
        try { //Ignore exceptions raised from client-side only slots that wont be found here. To be removed.
            if (player.level.isClientSide && slots.size() > id && (clickType == ClickType.PICKUP || clickType == ClickType.QUICK_MOVE)) {
                val slot = slots.get(id)
                if (!slot.isActive)
                    return handleGhostClick(slot, dragType, clickType, player)
            }
            super.clicked(id, dragType, clickType, player)
        } catch {
            case e:Exception => ItemStack.EMPTY
        }
    }

    private def handleGhostClick(slot:Slot, mouse:Int, clickType:ClickType, player:PlayerEntity):ItemStack =
    {
        val inSlot = slot.getItem
        val inCursor = player.inventory.getCarried
        if (!inCursor.isEmpty && !slot.mayPlace(inCursor)) return inCursor

        val stackable = InvWrapper.areItemsStackable(inSlot, inCursor)
        if (stackable)
        {
            if (!inSlot.isEmpty && inCursor.isEmpty) slot.set(ItemStack.EMPTY)
            else if (inSlot.isEmpty && !inCursor.isEmpty)
            {
                val newStack = inCursor.copy
                newStack.setCount(if (mouse == 0) math.min(inCursor.getCount, slot.getMaxStackSize) else 1)
                slot.set(newStack)
            }
            else if (!inSlot.isEmpty)
            {
                val toAdd = if (clickType == ClickType.QUICK_MOVE) 10 else 1
                if (mouse == 0) inSlot.setCount(math.min(slot.getMaxStackSize, inSlot.getCount+toAdd))
                else if (mouse == 1) inSlot.setCount(math.max(0, inSlot.getCount-toAdd))
                if (inSlot.getCount > 0) slot.set(inSlot)
                else slot.set(ItemStack.EMPTY)
            }
        }
        else
        {
            val newStack = inCursor.copy
            newStack.setCount(if (mouse == 0) math.min(inCursor.getCount, slot.getMaxStackSize) else 1)
            slot.set(newStack)
        }

        inCursor
    }

    override def quickMoveStack(player:PlayerEntity, i:Int):ItemStack =
    {
        var stack:ItemStack = ItemStack.EMPTY
        if (slots.size > i)
        {
            val slot = slots.get(i)
            if (slot != null && slot.hasItem)
            {
                stack = slot.getItem
                val manipStack = stack.copy

                if (!doMerge(player, manipStack, i) || stack.getCount == manipStack.getCount) return ItemStack.EMPTY

                if (manipStack.getCount <= 0) slot.set(ItemStack.EMPTY)
                else slot.set(manipStack)

                slot.onTake(player, stack)
            }
        }
        stack
    }

    def doMerge(player:PlayerEntity, stack:ItemStack, from:Int):Boolean = doMerge(stack, from)

    @deprecated("use doMerge(_:EntityPlayer, _:ItemStack, _:Int)")
    def doMerge(stack:ItemStack, from:Int):Boolean =
    {
        if (slots.size > 36) { //run standard impl on containers w/ player inventory
            if (slots.size-36 until slots.size contains from) { //if item is from player inventory...
                return tryMergeItemStack(stack, 0, slots.size-36, false) //merge to rest of container
            }
            else { //else if item from outside player inventory...
                if (tryMergeItemStack(stack, slots.size-36, slots.size-27, true)) return true //try merge to hotbar from back
                if (tryMergeItemStack(stack, slots.size-27, slots.size, true)) return true //then try player inventory from back
            }
        }

        false
    }

    def tryMergeItemStack(stack:ItemStack, start:Int, end:Int, reverse:Boolean) =
    {
        var flag1 = false
        var k = if(reverse) end-1 else start

        var slot:Slot = null
        var inslot:ItemStack = ItemStack.EMPTY
        if(stack.isStackable)
        {
            while(stack.getCount > 0 && (!reverse && k < end || reverse && k >= start))
            {
                slot = slots.get(k)
                inslot = slot.getItem
                if (/*slot.isEnabled && */!inslot.isEmpty && inslot.getItem == stack.getItem &&
                        ItemStack.tagMatches(stack, inslot))
                {
                    val space = math.min(slot.getMaxStackSize, stack.getMaxStackSize)-inslot.getCount
                    if (space >= stack.getCount)
                    {
                        inslot.setCount(inslot.getCount + stack.getCount)
                        stack.setCount(0)
                        slot.setChanged()
                        flag1 = true
                    }
                    else if (space > 0)
                    {
                        stack.setCount(stack.getCount - space)
                        inslot.setCount(inslot.getCount + space)
                        slot.setChanged()
                        flag1 = true
                    }
                }
                if(reverse) k -= 1 else k += 1
            }
        }

        if(stack.getCount > 0)
        {
            var k = if(reverse) end-1 else start

            import scala.util.control.Breaks._
            breakable
            {
                while(!reverse && k < end || reverse && k >= start)
                {
                    slot = slots.get(k)
                    inslot = slot.getItem
                    if(/*!slot.isEnabled && */inslot.isEmpty && slot.mayPlace(stack))
                    {
                        val space = math.min(slot.getMaxStackSize, stack.getMaxStackSize)
                        if (space >= stack.getCount)
                        {
                            slot.set(stack.copy)
                            slot.setChanged()
                            stack.setCount(0)
                            flag1 = true
                            break()
                        }
                        else
                        {
                            slot.set(stack.split(space))
                            slot.setChanged()
                            flag1 = true
                        }
                    }
                    if(reverse) k -= 1 else k += 1
                }
            }
        }

        flag1
    }

    //Hack to allow empty containers for use with guis without inventories
    override def setItem(slot:Int, stack:ItemStack)
    {
        if (slots.isEmpty || slots.size < slot) return
        else super.setItem(slot, stack)
    }
}

class Slot3(inv:IInventory, i:Int, x:Int, y:Int) extends Slot(inv, i, x, y) with TSlot3
{
    override def getMaxStackSize:Int = slotLimitCalculator()
    override def mayPickup(player:PlayerEntity):Boolean = canRemoveDelegate()
    override def mayPlace(stack:ItemStack):Boolean = canPlaceDelegate(stack)

    override def setChanged()
    {
        super.setChanged()
        slotChangeDelegate()
        slotChangeDelegate2()
    }
}

trait TSlot3 extends Slot
{
    var slotChangeDelegate = {() =>}
    var canRemoveDelegate = {() => !phantomSlot}
    var canPlaceDelegate = {(stack:ItemStack) => container.canPlaceItem(getSlotIndex, stack)}
    var slotLimitCalculator = {() => container.getMaxStackSize}

    var phantomSlot = false

    var slotChangeDelegate2 = {() =>} //used for container change delegate, do not set yourself!

    //additional methods required for this trait to work are located in class Slot3
}
