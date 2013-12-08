package mrtjp.projectred.core.inventory;

import java.util.ArrayList;
import java.util.List;

import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.Container;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.Slot;
import net.minecraft.item.ItemStack;


public class GhostContainer2 extends Container 
{
    protected IInventory playerInv;

    boolean allowDragging = true;
    boolean canInteract = true;

    public GhostContainer2(IInventory playerInv) {
        this.playerInv = playerInv;
    }

    @Override
    public boolean canDragIntoSlot(Slot par1Slot) {return allowDragging;}
    public GhostContainer2 setDragging(boolean flag) {allowDragging = flag; return this;}

    @Override
    public boolean canInteractWith(EntityPlayer p) {return canInteract;}
    public GhostContainer2 setInteraction(boolean flag) {canInteract = flag; return this;}

    public GhostContainer2 addPlayerInventory(int x, int y) {
        if (playerInv == null)
            return this;

        // pocket
        for (int row = 0; row < 3; row++)
            for (int column = 0; column < 9; column++)
                addSlotToContainer(new SlotExtended(playerInv, column + row * 9 + 9, x + column * 18, y + row * 18));
        // hotbar
        for (int i = 0; i < 9; i++)
            addSlotToContainer(new SlotExtended(playerInv, i, x + i * 18, y + 58));

        return this;
    }

    public GhostContainer2 addCustomSlot(SlotExtended slot) {
        addSlotToContainer(slot);
        return this;
    }

    @Override
    public ItemStack slotClick(int slotID, int mouseButton, int isShift, EntityPlayer player) {
        if (slotID >= 0 && slotID < inventorySlots.size()) {
            Slot s = (Slot) inventorySlots.get(slotID);
            if (s instanceof SlotExtended) {
                SlotExtended slot = (SlotExtended) inventorySlots.get(slotID);
                if (slot.handleClick())
                    return slot.slotClick(mouseButton, isShift, player);
            }
            return super.slotClick(slotID, mouseButton, isShift, player);
        }
        return null;
    }
    
    public List<IInventory> getAllInventories() {
        List<IInventory> list = new ArrayList<IInventory>();
        for (Slot s : (List<Slot>)inventorySlots)
            if (!list.contains(s.inventory))
                list.add(s.inventory);
        
        return list;
    }
    
    @Override
    public ItemStack transferStackInSlot(EntityPlayer player, int i) {
        Slot slot = (Slot) inventorySlots.get(i);
        if (slot ==  null)
            return null;
        
        List<IInventory> invList = getAllInventories();
        IInventory inv = null;
        
        int currentInvIndex = invList.indexOf(slot.inventory);
        if (currentInvIndex == -1)
            return null;
        
        for (int j = 1; j <= invList.size(); j++) {
            int invInd = (currentInvIndex + j) % invList.size();
            IInventory inv2 = invList.get(invInd);
            if (inv2 == slot.inventory)
                continue;
            
            inv = inv2;
            break;
        }

        if (inv == null)
            return null;
        
        if (slot.getHasStack()) {
            InventoryWrapper wrap = InventoryWrapper.wrapInventory(inv).setSlotsAll();
            ItemStack stack = slot.getStack();
            ItemStack stack2 = stack.copy();
            int added = wrap.injectItem(stack, true);
            if (added > 0)
                inv.onInventoryChanged();            

            stack.stackSize -= added;
            if (stack.stackSize <= 0)
                slot.putStack(null);
            else
                slot.onSlotChanged();
            return stack2;
        }
        
        return null;
    }
    
    @Override
    protected void retrySlotClick(int par1, int par2, boolean par3, EntityPlayer par4EntityPlayer) {
        
    }

    public static class SlotExtended extends Slot 
    {
        protected boolean allowRemove = true;
        protected boolean allowPlace = true;
        protected boolean enableGhosting = false;
        protected int limit = inventory.getInventoryStackLimit();
        protected ISlotController check;

        public SlotExtended(IInventory inv, int slot, int x, int y) {
            super(inv, slot, x, y);
        }

        public SlotExtended setRemoval(boolean flag) {allowRemove = flag; return this;}
        public SlotExtended setPlacement(boolean flag) {allowPlace = flag; return this;}
        public SlotExtended setGhosting(boolean flag) {enableGhosting = flag; allowRemove = allowPlace = !flag; return this;}
        public SlotExtended setLimit(int lim) {limit = lim; return this;}
        public SlotExtended setCheck(ISlotController c) {check = c; return this;}

        public boolean handleClick() {
            return enableGhosting;
        }

        public ItemStack slotClick(int mouseButton, int isShift, EntityPlayer player) {
            if (enableGhosting) return handleGhostClick(mouseButton, isShift, player);

            return player.inventory.getItemStack();
        }

        @Override
        public int getSlotStackLimit() {
            return limit;
        }

        @Override
        public boolean isItemValid(ItemStack stack) {
            return allowPlace && (check == null || check.canPlace(stack));
        }

        @Override
        public boolean canTakeStack(EntityPlayer player) {
            return allowRemove && (check == null || check.canTake());
        }

        private ItemStack handleGhostClick(int mouseButton, int isShift, EntityPlayer player) {
            ItemStack inSlot = getStack();
            ItemStack inCursor = player.inventory.getItemStack();
            if (InventoryWrapper.areItemsStackable(inSlot, inCursor)) {
                if (inSlot == null && inCursor != null) {
                    ItemStack newStack = inCursor.copy();
                    newStack.stackSize = mouseButton == 0 ? Math.min(inCursor.stackSize, getSlotStackLimit()) : 1;
                    putStack(newStack);
                } else if (inSlot != null && inCursor == null) {
                    int toAdd = isShift==1?10:1;

                    if (mouseButton == 0) // add
                    inSlot.stackSize = Math.min(getSlotStackLimit(), inSlot.stackSize+toAdd);
                    else if (mouseButton == 1) // subtract
                        inSlot.stackSize = Math.max(0, inSlot.stackSize-toAdd);

                    if (inSlot.stackSize > 0)
                        putStack(inSlot);
                    else
                        putStack(null);
                }
            } else { // Different inslot and incursor
                ItemStack newStack = inCursor.copy();
                newStack.stackSize = mouseButton == 0 ? Math.min(inCursor.stackSize, getSlotStackLimit()) : 1;
                putStack(newStack);
            }
            return inCursor;
        }
    }

    public static interface ISlotController 
    {
        public boolean canTake();
        public boolean canPlace(ItemStack stack);
    }
}
