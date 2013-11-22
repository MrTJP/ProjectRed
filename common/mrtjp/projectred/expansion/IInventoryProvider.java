package mrtjp.projectred.expansion;

import net.minecraft.inventory.IInventory;

public interface IInventoryProvider {
    public IInventory getInventory();
    public int getInterfacedSide();
}
