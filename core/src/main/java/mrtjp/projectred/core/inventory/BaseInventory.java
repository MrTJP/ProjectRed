package mrtjp.projectred.core.inventory;

import net.minecraft.inventory.Inventory;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.ListNBT;

/**
 * Simple extension of default vanilla Inventory class that allows for proper saving and loading
 * to a CompoundNBT. Default implementation does not load items back into their original slots.
 * <p>
 * Use BaseInventory#save and BaseInventory#load instead of Inventory#createTag and Inventory#fromTag
 */
public class BaseInventory extends Inventory {

    private static final String TAG_ITEMS = "items";
    private static final String TAG_SLOT = "slot";
    private static final String TAG_ITEM_COUNT = "item_count";

    public BaseInventory(int size) {
        super(size);
    }

    public void save(CompoundNBT tag) {
        ListNBT list = new ListNBT();
        for (int i = 0; i < getContainerSize(); i++) {
            ItemStack stack = getItem(i);
            if (!stack.isEmpty()) {
                CompoundNBT itemTag = new CompoundNBT();
                itemTag.putInt(TAG_SLOT, i);
                stack.save(itemTag);
                list.add(itemTag);
            }
        }
        tag.put(TAG_ITEMS, list);
        tag.putInt(TAG_ITEM_COUNT, list.size());
    }

    public void load(CompoundNBT tag) {
        clearContent();
        ListNBT list = tag.getList(TAG_ITEMS, 10);
        for (int i = 0; i < list.size(); i++) {
            CompoundNBT itemTag = list.getCompound(i);
            int slot = itemTag.contains("index") ? itemTag.getInt("index") : itemTag.getInt(TAG_SLOT); //TODO remove legacy support
            if (slot >= 0 && slot < getContainerSize()) {
                setItem(slot, ItemStack.of(itemTag));
            }
        }
    }

    public static int getItemCount(CompoundNBT tag) {
        return tag.contains(TAG_ITEM_COUNT) ? tag.getInt(TAG_ITEM_COUNT) : tag.getList(TAG_ITEMS, 10).size(); //TODO remove legacy support
    }
}
