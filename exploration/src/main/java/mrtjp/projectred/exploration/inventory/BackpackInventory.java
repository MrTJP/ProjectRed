package mrtjp.projectred.exploration.inventory;

import mrtjp.projectred.core.inventory.BaseInventory;
import mrtjp.projectred.exploration.item.BackpackItem;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.item.ItemStack;

public class BackpackInventory extends BaseInventory {

    public BackpackInventory(int inventorySize) {
        super(inventorySize);
    }

    public void loadInventoryFromMainHand(Inventory playerInventory) {

        ItemStack backpack = playerInventory.player.getMainHandItem();
        if (BackpackItem.isBackpack(backpack)) {
            CompoundTag inventoryTag = BackpackItem.getBackpackInventoryTag(backpack);
            load(inventoryTag);
            // Delete inventory from stack. It will be saved back once container closes
            BackpackItem.deleteBackpackInventory(backpack);
            BackpackItem.setBackpackOpenedFlag(backpack, true);
        }
    }

    public void saveInventoryToMainHand(Inventory playerInventory) {
        ItemStack backpack = playerInventory.player.getMainHandItem();
        if (BackpackItem.isBackpack(backpack)) {
            CompoundTag inventoryTag = new CompoundTag();
            save(inventoryTag);
            BackpackItem.saveBackpackInventory(backpack, inventoryTag);
            BackpackItem.setBackpackOpenedFlag(backpack, false);
        }
    }

    @Override
    public boolean canPlaceItem(int slot, ItemStack stack) {
        return BackpackItem.isItemAllowedInBackpack(stack);
    }
}
