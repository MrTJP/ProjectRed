package mrtjp.projectred.core;

import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.Slot;
import net.minecraft.item.ItemStack;

public class RestrictedSlot extends Slot {

	private final int ItemID;
	private final ISlotCheck slotCheck;
	
	public RestrictedSlot(IInventory iinventory, int i, int j, int k, int ItemID) {
		super(iinventory, i, j, k);
		this.ItemID = ItemID;
		slotCheck = null;
	}
	
    public RestrictedSlot(IInventory iinventory, int i, int j, int k, ISlotCheck slotCheck) {
    	super(iinventory, i, j, k);
    	this.ItemID = -1;
		this.slotCheck = slotCheck;
	}

	/**
     * Check if the stack is a valid item for this slot. Always true beside for the armor slots.
     */
    @Override
	public boolean isItemValid(ItemStack stack) {	
    	if(slotCheck == null) {
    		return stack.itemID == ItemID;
    	} else {
    		return slotCheck.isSlotAllowed(stack);
    	}
    }
    
    public interface ISlotCheck {
    	public boolean isSlotAllowed(ItemStack stack);
    }
}
