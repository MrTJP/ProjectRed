package mrtjp.projectred.utils.gui;

import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.IInventory;

public class FinalRestrictedSlot extends RestrictedSlot {
	int limit;
	public FinalRestrictedSlot(IInventory iinventory, int i, int j, int k, int ItemID, int stackLimit) {
		super(iinventory, i, j, k, ItemID);
		this.limit = stackLimit;
	}
	
    public FinalRestrictedSlot(IInventory iinventory, int i, int j, int k, ISlotCheck slotCheck, int stackLimit) {
    	super(iinventory, i, j, k, slotCheck);
		this.limit = stackLimit;
	}
    
    /**
     * Return whether this slot's stack can be taken from this slot.
     */
    @Override
    public boolean canTakeStack(EntityPlayer par1EntityPlayer) {
        return false;
    }
    
    /**
     * Returns the maximum stack size for a given slot (usually the same as getInventoryStackLimit(), but 1 in the case
     * of armor slots)
     */
    @Override
    public int getSlotStackLimit() {
    	return limit;
    }
}
