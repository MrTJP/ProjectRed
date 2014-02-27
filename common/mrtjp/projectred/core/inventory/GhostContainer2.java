package mrtjp.projectred.core.inventory;

import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.Container;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.Slot;
import net.minecraft.item.ItemStack;

import java.util.ArrayList;
import java.util.List;

public class GhostContainer2 extends Container
{
    protected IInventory playerInv;

    boolean allowDragging = true;
    boolean canInteract = true;

    public GhostContainer2(IInventory playerInv)
    {
        this.playerInv = playerInv;
    }

    @Override
    public boolean canDragIntoSlot(Slot par1Slot)
    {
        return allowDragging;
    }

    public GhostContainer2 setDragging(boolean flag)
    {
        allowDragging = flag;
        return this;
    }

    @Override
    public boolean canInteractWith(EntityPlayer p)
    {
        return canInteract;
    }

    public GhostContainer2 setInteraction(boolean flag)
    {
        canInteract = flag;
        return this;
    }

    public GhostContainer2 addPlayerInventory(int x, int y)
    {
        if (playerInv == null)
            return this;

        // pocket
        for (int row = 0; row < 3; row++)
            for (int column = 0; column < 9; column++)
                addCustomSlot(new SlotExtended(playerInv, column + row * 9 + 9, x + column * 18, y + row * 18));
        // hotbar
        for (int i = 0; i < 9; i++)
            addCustomSlot(new SlotExtended(playerInv, i, x + i * 18, y + 58));

        return this;
    }

    public GhostContainer2 addCustomSlot(SlotExtended slot)
    {
        addSlotToContainer(slot);
        return this;
    }

    @Override
    public ItemStack slotClick(int slotID, int mouseButton, int isShift, EntityPlayer player)
    {
        if (slotID >= 0 && slotID < inventorySlots.size())
        {
            Slot s = (Slot) inventorySlots.get(slotID);
            if (s instanceof SlotExtended)
            {
                SlotExtended slot = (SlotExtended) inventorySlots.get(slotID);
                if (slot.isGhostingEnabled())
                    return handleGhostClick(slot, mouseButton, isShift, player);
            }
            return super.slotClick(slotID, mouseButton, isShift, player);
        }
        return null;
    }

    private ItemStack handleGhostClick(SlotExtended slot, int mouseButton, int isShift, EntityPlayer player)
    {
        ItemStack inSlot = slot.getStack();
        ItemStack inCursor = player.inventory.getItemStack();
        boolean stackable = InvWrapper.areItemsStackable(inSlot, inCursor);

        if (stackable)
        {
            if (inSlot != null && inCursor == null)
                slot.putStack(null);
            else if (inSlot == null && inCursor != null)
            {
                ItemStack newStack = inCursor.copy();
                newStack.stackSize = mouseButton == 0 ? Math.min(inCursor.stackSize, slot.getSlotStackLimit()) : 1;
                slot.putStack(newStack);
            }
            else if (inSlot != null)
            {
                int toAdd = isShift == 1 ? 10 : 1;

                if (mouseButton == 0) // add
                    inSlot.stackSize = Math.min(slot.getSlotStackLimit(), inSlot.stackSize + toAdd);
                else if (mouseButton == 1) // subtract
                    inSlot.stackSize = Math.max(0, inSlot.stackSize - toAdd);

                if (inSlot.stackSize > 0)
                    slot.putStack(inSlot);
                else
                    slot.putStack(null);
            }
        }
        else
        { // Different inslot and incursor
            ItemStack newStack = inCursor.copy();
            newStack.stackSize = mouseButton == 0 ? Math.min(inCursor.stackSize, slot.getSlotStackLimit()) : 1;
            slot.putStack(newStack);
        }
        return inCursor;
    }


    private List<IInventory> getAllInventories()
    {
        List<IInventory> list = new ArrayList<IInventory>();
        for (Slot s : (List<Slot>) inventorySlots)
            if (!list.contains(s.inventory))
                list.add(s.inventory);

        return list;
    }

    @Override
    public ItemStack transferStackInSlot(EntityPlayer player, int i)
    {
        Slot slot = (Slot) inventorySlots.get(i);
        if (slot == null)
            return null;

        List<IInventory> invList = getAllInventories();
        IInventory inv = null;

        int currentInvIndex = invList.indexOf(slot.inventory);
        if (currentInvIndex == -1)
            return null;

        for (int j = 1; j <= invList.size(); j++)
        {
            int invInd = (currentInvIndex + j) % invList.size();
            IInventory inv2 = invList.get(invInd);
            if (inv2 == slot.inventory)
                continue;

            inv = inv2;
            break;
        }

        if (inv == null)
            return null;

        if (slot.getHasStack())
        {
            InvWrapper wrap = InvWrapper.wrap(inv).setSlotsAll(); // TODO set from method
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
    protected void retrySlotClick(int par1, int par2, boolean par3, EntityPlayer par4EntityPlayer)
    {
    }

    public static class SlotExtended extends Slot
    {
        protected boolean allowRemove = true;
        protected boolean allowPlace = true;
        protected boolean enableGhosting = false;
        protected int limit = inventory.getInventoryStackLimit();
        protected ISlotController check;

        public SlotExtended(IInventory inv, int slot, int x, int y)
        {
            super(inv, slot, x, y);
        }

        public SlotExtended setRemoval(boolean flag)
        {
            allowRemove = flag;
            return this;
        }

        public SlotExtended setPlacement(boolean flag)
        {
            allowPlace = flag;
            return this;
        }

        public SlotExtended setGhosting(boolean flag)
        {
            enableGhosting = flag;
            allowRemove = allowPlace = !flag;
            return this;
        }

        public SlotExtended setLimit(int lim)
        {
            limit = lim;
            return this;
        }

        public SlotExtended setCheck(ISlotController c)
        {
            check = c;
            return this;
        }

        public boolean isGhostingEnabled()
        {
            return enableGhosting;
        }

        @Override
        public int getSlotStackLimit()
        {
            return limit;
        }

        @Override
        public boolean isItemValid(ItemStack stack)
        {
            return allowPlace && (check == null || check.canPlace(this, stack));
        }

        @Override
        public boolean canTakeStack(EntityPlayer player)
        {
            return allowRemove && (check == null || check.canTake(this));
        }
    }

    public static interface ISlotController
    {
        public boolean canTake(SlotExtended slot);

        public boolean canPlace(SlotExtended slot, ItemStack stack);

        public static class InventoryRulesController implements ISlotController
        {
            public static InventoryRulesController instance = new InventoryRulesController();

            @Override
            public boolean canTake(SlotExtended slot)
            {
                return true;
            }

            @Override
            public boolean canPlace(SlotExtended slot, ItemStack stack)
            {
                return slot.inventory.isItemValidForSlot(slot.getSlotIndex(), stack);
            }
        }
    }
}
