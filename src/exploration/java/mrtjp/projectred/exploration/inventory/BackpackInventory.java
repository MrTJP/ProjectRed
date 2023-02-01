package mrtjp.projectred.exploration.inventory;

import mrtjp.projectred.core.inventory.BaseInventory;
import mrtjp.projectred.exploration.item.BackpackItem;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;

public class BackpackInventory extends BaseInventory {

    public BackpackInventory(int inventorySize) {
        super(inventorySize);
    }

    public void loadInventoryFromMainHand(PlayerInventory playerInventory) {

        ItemStack backpack = playerInventory.player.getMainHandItem();
        if (BackpackItem.isBackpack(backpack)) {
            CompoundNBT inventoryTag = BackpackItem.getBackpackInventoryTag(backpack);
            load(inventoryTag);
            // Delete inventory from stack. It will be saved back once container closes
            BackpackItem.deleteBackpackInventory(backpack);
        }
    }

    public void saveInventoryToMainHand(PlayerInventory playerInventory) {
        ItemStack backpack = playerInventory.player.getMainHandItem();
        if (BackpackItem.isBackpack(backpack)) {
            CompoundNBT inventoryTag = new CompoundNBT();
            save(inventoryTag);
            BackpackItem.saveBackpackInventory(backpack, inventoryTag);
        }
    }

    @Override
    public boolean canPlaceItem(int slot, ItemStack stack) {
        return BackpackItem.isItemAllowedInBackpack(stack);
    }
}
