package mrtjp.projectred.core;

import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.Slot;

public class GuiUnmodifiableSlot extends Slot {
    public GuiUnmodifiableSlot(IInventory par1iInventory, int par2, int par3, int par4) {
        super(par1iInventory, par2, par3, par4);
    }

    public GuiUnmodifiableSlot(Slot slot) {
        super(slot.inventory, slot.getSlotIndex(), slot.xDisplayPosition, slot.yDisplayPosition);
    }

    @Override
    public boolean canTakeStack(EntityPlayer par1EntityPlayer) {
        return false;
    }
}
