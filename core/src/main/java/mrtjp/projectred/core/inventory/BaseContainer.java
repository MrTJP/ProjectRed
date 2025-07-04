package mrtjp.projectred.core.inventory;

import mrtjp.projectred.core.ProjectRedCore;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.item.ItemStack;

/**
 * Simple extension of default vanilla Inventory class that allows for proper saving and loading
 * to a CompoundNBT. Default implementation does not load items back into their original slots.
 * <p>
 * Use {@link BaseContainer#save} and {@link BaseContainer#load} instead of Inventory#createTag and Inventory#fromTag
 */
public class BaseContainer extends SimpleContainer {

    private static final String TAG_ITEMS = "items";
    private static final String TAG_SLOT = "slot";

    public BaseContainer(int size) {
        super(size);
    }

    public BaseContainer(ItemStack... stacks) {
        super(stacks);
    }

    public CompoundTag save(CompoundTag tag, HolderLookup.Provider lookupProvider) {
        ListTag list = new ListTag();
        for (int i = 0; i < getContainerSize(); i++) {
            ItemStack stack = getItem(i);
            if (!stack.isEmpty()) {
                CompoundTag itemTag = new CompoundTag();
                itemTag.putInt(TAG_SLOT, i);
                list.add(stack.save(lookupProvider, itemTag));
            }
        }
        tag.put(TAG_ITEMS, list);

        return tag;
    }

    public void load(CompoundTag tag, HolderLookup.Provider lookupProvider) {
        clearContent();
        ListTag list = tag.getList(TAG_ITEMS, 10);
        for (int i = 0; i < list.size(); i++) {
            CompoundTag itemTag = list.getCompound(i);
            int slot = itemTag.getInt(TAG_SLOT);
            if (slot >= 0 && slot < getContainerSize()) {
                ItemStack.parse(lookupProvider, itemTag)
                        .ifPresent(stack -> setItem(slot, stack));
            }
        }
    }

    public void saveTo(CompoundTag parent, String key, HolderLookup.Provider lookupProvider) {
        parent.put(key, save(new CompoundTag(), lookupProvider));
    }

    public void loadFrom(CompoundTag parent, String key, HolderLookup.Provider lookupProvider) {
        // Check if list-format tags are present
        ListTag itemList = parent.getList(key, 10);
        if (!itemList.isEmpty()) {
            fromTag(itemList, lookupProvider);
            ProjectRedCore.LOGGER.warn("Inventory {} loaded from non-ordered data. Its contents may have shuffled", this);
        } else {
            // Otherwise, use new compound format
            load(parent.getCompound(key), lookupProvider);
        }
    }

    public static int getItemCount(CompoundTag tag) {
        return tag.getList(TAG_ITEMS, 10).size();
    }

    //region Deprecate vanilla save/load as it does not load back to correct slots
    @Override
    @Deprecated // Use saveTo or save
    public ListTag createTag(HolderLookup.Provider lookupProvider) {
        return super.createTag(lookupProvider);
    }

    @Override
    @Deprecated // Use loadFrom or load
    public void fromTag(ListTag pContainerNbt, HolderLookup.Provider lookupProvider) {
        super.fromTag(pContainerNbt, lookupProvider);
    }
    //endregion
}
