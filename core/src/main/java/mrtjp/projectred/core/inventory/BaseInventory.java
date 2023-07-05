package mrtjp.projectred.core.inventory;

import mrtjp.projectred.core.ProjectRedCore;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.item.ItemStack;

/**
 * Simple extension of default vanilla Inventory class that allows for proper saving and loading
 * to a CompoundNBT. Default implementation does not load items back into their original slots.
 * <p>
 * Use {@link BaseInventory#save} and {@link BaseInventory#load} instead of Inventory#createTag and Inventory#fromTag
 */
public class BaseInventory extends SimpleContainer {

    private static final String TAG_ITEMS = "items";
    private static final String TAG_SLOT = "slot";
    private static final String TAG_ITEM_COUNT = "item_count";

    public BaseInventory(int size) {
        super(size);
    }

    public CompoundTag save(CompoundTag tag) {
        ListTag list = new ListTag();
        for (int i = 0; i < getContainerSize(); i++) {
            ItemStack stack = getItem(i);
            if (!stack.isEmpty()) {
                CompoundTag itemTag = new CompoundTag();
                itemTag.putInt(TAG_SLOT, i);
                stack.save(itemTag);
                list.add(itemTag);
            }
        }
        tag.put(TAG_ITEMS, list);
        tag.putInt(TAG_ITEM_COUNT, list.size());

        return tag;
    }

    public void load(CompoundTag tag) {
        clearContent();
        ListTag list = tag.getList(TAG_ITEMS, 10);
        for (int i = 0; i < list.size(); i++) {
            CompoundTag itemTag = list.getCompound(i);
            int slot = itemTag.contains("index") ? itemTag.getInt("index") : itemTag.getInt(TAG_SLOT); //TODO remove legacy support
            if (slot >= 0 && slot < getContainerSize()) {
                setItem(slot, ItemStack.of(itemTag));
            }
        }
    }

    public void saveTo(CompoundTag parent, String key) {
        parent.put(key, save(new CompoundTag()));
    }

    public void loadFrom(CompoundTag parent, String key) {
        // Check if list-format tags are present
        ListTag itemList = parent.getList(key, 10);
        if (!itemList.isEmpty()) {
            ProjectRedCore.LOGGER.warn("Inventory {} loaded from non-ordered data. Its contents may have shuffled", this);
            fromTag(itemList);
        } else {
            // Otherwise, use new compound format
            load(parent.getCompound(key));
        }
    }

    public static int getItemCount(CompoundTag tag) {
        return tag.contains(TAG_ITEM_COUNT) ? tag.getInt(TAG_ITEM_COUNT) : tag.getList(TAG_ITEMS, 10).size(); //TODO remove legacy support
    }
}
