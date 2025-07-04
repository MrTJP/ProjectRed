package mrtjp.projectred.exploration.inventory;

import mrtjp.projectred.core.inventory.BaseContainer;
import mrtjp.projectred.exploration.item.BackpackItem;
import net.minecraft.world.item.ItemStack;

import java.util.List;

public class BackpackInventory extends BaseContainer {

    public BackpackInventory(int inventorySize) {
        super(inventorySize);
    }

    public BackpackInventory(ItemStack... stacks) {
        super(stacks);
    }

    public BackpackInventory(List<ItemStack> inventory) {
        super(inventory.toArray(new ItemStack[0]));
    }

    @Override
    public boolean canPlaceItem(int slot, ItemStack stack) {
        return BackpackItem.isItemAllowedInBackpack(stack);
    }
}
