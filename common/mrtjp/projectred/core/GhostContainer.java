package mrtjp.projectred.core;

import mrtjp.projectred.core.GuiRestrictedSlot.ISlotCheck;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.InventoryPlayer;
import net.minecraft.inventory.Container;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.Slot;
import net.minecraft.item.ItemStack;

public class GhostContainer extends Container {

    protected IInventory _playerInventory;
    protected IInventory _inv;

    public GhostContainer(IInventory playerInventory, IInventory inv) {
        _playerInventory = playerInventory;
        _inv = inv;
    }

    @Override
    public boolean canInteractWith(EntityPlayer entityplayer) {
        return true;
    }

    /***
     * Adds all slots for the player inventory and hotbar
     * 
     * @param xOffset
     * @param yOffset
     */
    public void addNormalSlotsForPlayerInventory(int xOffset, int yOffset) {
        addNormalSlotsForPlayerInventory(xOffset, yOffset, -1);
    }

    public void addNormalSlotsForPlayerInventory(int xOffset, int yOffset, int staticSlot) {
        if (_playerInventory == null)
            return;
        // pocket
        for (int row = 0; row < 3; row++)
            for (int column = 0; column < 9; column++)
                addSlotToContainer(new Slot(_playerInventory, column + row * 9 + 9, xOffset + column * 18, yOffset + row * 18));

        // hotbar
        for (int i1 = 0; i1 < 9; i1++)
            if (i1 == staticSlot)
                addSlotToContainer(new GuiUnmodifiableSlot(_playerInventory, i1, xOffset + i1 * 18, yOffset + 58));
            else
                addSlotToContainer(new Slot(_playerInventory, i1, xOffset + i1 * 18, yOffset + 58));
    }

    /**
     * Normal vanilla slot.
     */
    public void addNormalSlot(int slotId, IInventory inventory, int xCoord, int yCoord) {
        addSlotToContainer(new Slot(inventory, slotId, xCoord, yCoord));
    }

    /**
     * A ghost slot will not take items.
     */
    public void addGhostSlot(int slotId, int xCoord, int yCoord) {
        addSlotToContainer(new GuiGhostSlot(_inv, slotId, xCoord, yCoord));
    }

    /**
     * Slot that will only accept the given ID.
     */
    public void addRestrictedSlot(int slotId, IInventory inventory, int xCoord, int yCoord, int ItemID) {
        addSlotToContainer(new GuiRestrictedSlot(inventory, slotId, xCoord, yCoord, ItemID));
    }

    /**
     * A restricted slot that will only accept the given ID and will not allow
     * removal of the stack.
     */
    public void addFinalRestrictedSlot(int slotId, IInventory inventory, int xCoord, int yCoord, int ItemID, int stackLimit) {
        addSlotToContainer(new GuiFinalRestrictedSlot(inventory, slotId, xCoord, yCoord, ItemID, stackLimit));
    }

    /**
     * Slot that will only accept if slotCheck allows it.
     */
    public void addRestrictedSlot(int slotId, IInventory inventory, int xCoord, int yCoord, ISlotCheck slotCheck) {
        addSlotToContainer(new GuiRestrictedSlot(inventory, slotId, xCoord, yCoord, slotCheck));
    }

    /**
     * Slot that will only accept if slotCheck allows it and wont allow removal.
     */
    public void addFinalRestrictedSlot(int slotId, IInventory inventory, int xCoord, int yCoord, ISlotCheck slotCheck, int stackLimit) {
        addSlotToContainer(new GuiFinalRestrictedSlot(inventory, slotId, xCoord, yCoord, slotCheck, stackLimit));
    }

    /**
     * Slot that will not allow removal.
     */
    public void addUnmodifiableSlot(int slotId, IInventory inventory, int xCoord, int yCoord) {
        addSlotToContainer(new GuiUnmodifiableSlot(inventory, slotId, xCoord, yCoord));
    }

    /**
     * Disable shift-clicking to transfer items
     */
    @Override
    public ItemStack transferStackInSlot(EntityPlayer player, int i) {
        ItemStack itemstack = null;
        Slot slot = (Slot) this.inventorySlots.get(i);

        if (slot != null && slot.getHasStack()) {
            ItemStack itemstack1 = slot.getStack();
            itemstack = itemstack1.copy();

            if (i < 27)
                if (!this.mergeItemStack(itemstack1, 27, this.inventorySlots.size(), true))
                    return null;
            else if (!this.mergeItemStack(itemstack1, 0, 27, false))
                return null;

            if (itemstack1.stackSize == 0)
                slot.putStack((ItemStack) null);
            else
                slot.onSlotChanged();
        }
        return itemstack;
    }

    protected boolean mergeItemStack(ItemStack stack, int startSlot, int endSlot, boolean doBackwards) {
        boolean flag1 = false;
        int k = startSlot;

        if (doBackwards)
            k = endSlot - 1;

        Slot slot;
        ItemStack itemstack1;

        if (stack.isStackable()) {
            while (stack.stackSize > 0 && (!doBackwards && k < endSlot || doBackwards && k >= startSlot)) {
                slot = (Slot) this.inventorySlots.get(k);
                itemstack1 = slot.getStack();

                if (slot.isItemValid(stack)) {
                    if (itemstack1 != null && itemstack1.itemID == stack.itemID && (!stack.getHasSubtypes() || stack.getItemDamage() == itemstack1.getItemDamage()) && ItemStack.areItemStackTagsEqual(stack, itemstack1)) {
                        int l = itemstack1.stackSize + stack.stackSize;

                        if (l <= stack.getMaxStackSize()) {
                            stack.stackSize = 0;
                            itemstack1.stackSize = l;
                            slot.onSlotChanged();
                            flag1 = true;
                        } else if (itemstack1.stackSize < stack.getMaxStackSize()) {
                            stack.stackSize -= stack.getMaxStackSize() - itemstack1.stackSize;
                            itemstack1.stackSize = stack.getMaxStackSize();
                            slot.onSlotChanged();
                            flag1 = true;
                        }
                    }
                }
                if (doBackwards)
                    --k;
                else
                    ++k;
            }
        }

        if (stack.stackSize > 0) {
            if (doBackwards)
                k = endSlot - 1;
            else
                k = startSlot;

            while (!doBackwards && k < endSlot || doBackwards && k >= startSlot) {
                slot = (Slot) this.inventorySlots.get(k);
                itemstack1 = slot.getStack();

                if (slot.isItemValid(stack)) {
                    if (itemstack1 == null) {
                        slot.putStack(stack.copy());
                        slot.onSlotChanged();
                        stack.stackSize = 0;
                        flag1 = true;
                        break;
                    }
                }
                if (doBackwards)
                    --k;
                else
                    ++k;
            }
        }

        return flag1;
    }

    /**
     * Clone/clear itemstacks for items
     */
    @Override
    public ItemStack slotClick(int slotId, int mouseButton, int isShift, EntityPlayer entityplayer) {
        if (slotId < 0)
            return super.slotClick(slotId, mouseButton, isShift, entityplayer);

        InventoryPlayer inventoryplayer = entityplayer.inventory;
        ItemStack currentlyEquippedStack = inventoryplayer.getItemStack();
        Slot slot = (Slot) inventorySlots.get(slotId);
        if (slot == null || (!(slot instanceof GuiGhostSlot) && !(slot instanceof GuiUnmodifiableSlot))) {
            ItemStack stack1 = super.slotClick(slotId, mouseButton, isShift, entityplayer);
            return stack1;
        }

        // Dont let modify an unmodifiable slot
        if (slot instanceof GuiUnmodifiableSlot)
            return currentlyEquippedStack;

        // Use ghost items if its a ghost slot.
        if (slot instanceof GuiGhostSlot) {
            ItemStack currentItem = currentlyEquippedStack;
            ItemStack slotItem = slot.getStack();
            if (BasicUtils.areStacksTheSame(currentItem, slotItem)) {
                int counter = isShift == 1 ? 10 : 1;
                if (mouseButton == 1) {
                    if (slot.getStack().stackSize + counter <= slot.getSlotStackLimit())
                        slot.getStack().stackSize += counter;
                    else
                        slot.getStack().stackSize = slot.getSlotStackLimit();
                    slot.inventory.onInventoryChanged();
                    return currentlyEquippedStack;
                }
                if (mouseButton == 0) {
                    if (slot.getStack().stackSize - counter > 0) {
                        slot.getStack().stackSize -= counter;
                        slot.inventory.onInventoryChanged();
                    } else
                        slot.putStack(null);
                    return currentlyEquippedStack;
                }
            } else {
                slot.putStack(currentlyEquippedStack.copy());
                if (slot.getStack().stackSize > slot.getSlotStackLimit()) {
                    slot.getStack().stackSize = slot.getSlotStackLimit();
                    slot.inventory.onInventoryChanged();
                }
            }
            if (currentlyEquippedStack == null) {
                if (slot.getStack() != null && mouseButton == 1) {
                    if (isShift == 1) {
                        slot.getStack().stackSize = Math.min(slot.getSlotStackLimit(), slot.getStack().stackSize * 2);
                        slot.inventory.onInventoryChanged();
                    } else {
                        slot.getStack().stackSize /= 2;
                        slot.inventory.onInventoryChanged();
                    }
                } else
                    slot.putStack(null);
                return currentlyEquippedStack;
            }
            if (!slot.getHasStack()) {
                slot.putStack(currentlyEquippedStack.copy());
                if (mouseButton == 1)
                    slot.getStack().stackSize = 1;
                if (slot.getStack().stackSize > slot.getSlotStackLimit())
                    slot.getStack().stackSize = slot.getSlotStackLimit();

                slot.inventory.onInventoryChanged();
                return currentlyEquippedStack;
            }
        }

        return currentlyEquippedStack;
    }

    @Override
    public void onContainerClosed(EntityPlayer par1EntityPlayer) {
        super.onContainerClosed(par1EntityPlayer);
    }

    @Override
    protected void retrySlotClick(int i, int j, boolean flag, EntityPlayer entityplayer) {
        super.retrySlotClick(i, j, flag, entityplayer);
    }

    @Override
    public Slot getSlotFromInventory(IInventory par1IInventory, int par2) {
        Slot s = super.getSlotFromInventory(par1IInventory, par2);
        if (s != null)
            return s;
        if (inventorySlots.isEmpty() && par1IInventory == _playerInventory) {
            s = new Slot(_playerInventory, par2, 0, 0);
            s.slotNumber = par2;
            return s;
        }
        return null;
    }

    @Override
    public void putStackInSlot(int slotID, ItemStack stack) {
        if (inventorySlots.isEmpty()) {
            _playerInventory.setInventorySlotContents(slotID, stack);
            _playerInventory.onInventoryChanged();
            return;
        }
        super.putStackInSlot(slotID, stack);
    }
}
