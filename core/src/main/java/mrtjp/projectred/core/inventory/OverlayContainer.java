package mrtjp.projectred.core.inventory;

import net.minecraft.world.Container;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.player.StackedContents;
import net.minecraft.world.inventory.StackedContentsCompatible;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

/**
 * Container that overlays a real container to allow item modification without effecting the underlying container.
 * The data can then be written back if desired.
 * <p>
 * ItemStacks are only copied on access. If underlying is changed before first access, the new change will
 * reflect in this layer.
 */
public class OverlayContainer implements Container, StackedContentsCompatible {

    private final ArrayList<OverlayItem> items;
    private final int maxStackSize;

    private OverlayContainer(ArrayList<OverlayItem> items, int maxStackSize) {
        this.items = items;
        this.maxStackSize = maxStackSize;
    }

    //region Container

    @Override
    public int getContainerSize() {
        return items.size();
    }

    @Override
    public boolean isEmpty() {
        for (OverlayItem overlay : items) {
            if (!overlay.getItemNoCopy().isEmpty()) {
                return false;
            }
        }
        return true;
    }

    @Override
    public ItemStack getItem(int i) {
        return items.get(i).getItem();
    }

    @Override
    public ItemStack removeItem(int i, int amount) {
        if (amount < 0) {
            return ItemStack.EMPTY;
        }
        var overlay = items.get(i);
        if (overlay.getItemNoCopy().isEmpty()) {
            return ItemStack.EMPTY;
        }
        var result = overlay.getItem().split(amount);
        setChanged();
        return result;
    }

    @Override
    public ItemStack removeItemNoUpdate(int i) {
        var overlay = items.get(i);
        if (overlay.getItemNoCopy().isEmpty()) {
            return ItemStack.EMPTY;
        }
        var result = overlay.getItem();
        overlay.setItem(ItemStack.EMPTY);
        return result;
    }

    @Override
    public void setItem(int i, ItemStack stack) {
        var overlay = items.get(i);
        overlay.setItem(stack);
        stack.limitSize(getMaxStackSize(stack));
    }

    @Override
    public void setChanged() {
    }

    @Override
    public boolean stillValid(Player player) {
        return false;
    }

    @Override
    public void clearContent() {
        for (OverlayItem overlay : items) {
            overlay.setItem(ItemStack.EMPTY);
        }
        setChanged();
    }

    @Override
    public int getMaxStackSize() {
        return maxStackSize;
    }

    // Re-implement default to prevent unnecessary copy
    @Override
    public int countItem(Item item) {
        int result = 0;
        for (OverlayItem overlay : items) {
            var stack = overlay.getItemNoCopy();
            if (stack.getItem().equals(item)) {
                result += stack.getCount();
            }
        }
        return result;
    }
    //endregion

    //region StackedContentsCompatible
    @Override
    public void fillStackedContents(StackedContents contents) {
        for (OverlayItem overlay : items) {
            contents.accountStack(overlay.getItemNoCopy());
        }
    }
    //endregion

    //region Overlay utils

    /**
     * Drop any changes and go back to underlying container's state
     */
    public void clearChanges() {
        for (OverlayItem overlay : items) {
            overlay.clear();
        }
    }

    /**
     * Write any changes back to the underlying containers.
     */
    public void commitChanges() {
        for (OverlayItem overlay : items) {
            overlay.commit();
        }
    }

    /**
     * Force-copies all items into this layer even if unchanged.
     */
    public void copyUp() {
        for (OverlayItem overlay : items) {
            overlay.getItem();
        }
    }

    /**
     * Create another overlay layer.
     *
     * @return An overlay on top of this overlay
     */
    public OverlayContainer newLayer() {
        return new Builder().addItems(this).build();
    }

    public static OverlayContainer.Builder builder() {
        return new Builder();
    }

    public static OverlayContainer of(Container container) {
        return new Builder().addItems(container).build();
    }
    //endregion

    public static class Builder {

        private ArrayList<OverlayItem> items = new ArrayList<>();
        private Set<Integer> maxStackSizes = new HashSet<>();

        private Builder() {
        }

        public Builder addItems(Container container) {
            return addItems(container, 0, container.getContainerSize());
        }

        public Builder addItems(Container src, int srcPos, int length) {
            for (int i = srcPos; i < srcPos + length; i++) {
                addItem(src, i);
            }
            return this;
        }

        public Builder addItem(Container src, int srcPos) {
            items.add(new OverlayItem(src, srcPos));
            maxStackSizes.add(src.getMaxStackSize());
            return this;
        }

        public OverlayContainer build() {
            if (maxStackSizes.size() > 1) {
                throw new RuntimeException("Cannot overlay containers with different max stack sizes: " + maxStackSizes);
            }
            int maxStackSize = maxStackSizes.isEmpty() ? 64 : maxStackSizes.iterator().next();
            return new OverlayContainer(items, maxStackSize);
        }
    }

    private static class OverlayItem {

        private final Container src;
        private final int srcPos;

        @Nullable
        private ItemStack item = null;

        public OverlayItem(Container src, int srcPos) {
            this.src = src;
            this.srcPos = srcPos;
        }

        public ItemStack getItem() {
            // Copy-on-read
            if (item == null) {
                item = src.getItem(srcPos).copy();
            }
            return item;
        }

        // INTERNAL USE ONLY
        public ItemStack getItemNoCopy() {
            if (item == null) {
                return src.getItem(srcPos);
            }
            return item;
        }

        public void setItem(ItemStack item) {
            this.item = item;
        }

        public void clear() {
            item = null;
        }

        public void commit() {
            if (item != null) {
                src.setItem(srcPos, item);
            }
        }

        @Override
        public String toString() {
            // String includes src, srcPos, upper and lower item
            return String.format("OverlayItem(src=%s, srcPos=%d, lowerItem=%s, upperItem=%s)", src, srcPos, getItemNoCopy(), item);
        }
    }
}
