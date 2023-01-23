package mrtjp.projectred.lib;

import net.minecraft.inventory.IInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.util.NonNullList;

public class InventoryLib {

    public static void injectItemStack(IInventory inventory, ItemStack stack, boolean reverse) {
        injectItemStack(inventory, stack, 0, inventory.getContainerSize(), reverse);
    }

    public static void injectItemStack(IInventory inventory, ItemStack stack, int startIndex, int endIndex, boolean reverse) {
        injectItemStack(inventory, stack, startIndex, endIndex, reverse, true);
        if (!stack.isEmpty()) {
            injectItemStack(inventory, stack, startIndex, endIndex, reverse, false);
        }
    }

    public static boolean injectAllItemStacks(IInventory inventory, NonNullList<ItemStack> stacks, boolean reverse) {
        return injectAllItemStacks(inventory, stacks, 0, inventory.getContainerSize(), reverse);
    }

    public static boolean injectAllItemStacks(IInventory inventory, NonNullList<ItemStack> stacks, int startIndex, int endIndex, boolean reverse) {

        boolean allInjected = true;

        for (int i = 0; i < stacks.size(); i++) {
            ItemStack stack = stacks.get(i);
            injectItemStack(inventory, stack, startIndex, endIndex, reverse);

            if (stack.isEmpty()) {
                stacks.set(i, ItemStack.EMPTY);
            } else {
                allInjected = false;
            }
        }

        return allInjected;
    }

    private static void injectItemStack(IInventory inventory, ItemStack stack, int startIndex, int endIndex, boolean reverse, boolean mergeOnly) {

        for (int i = startIndex; i < endIndex; i++) {
            int index = reverse ? endIndex - i - 1 : i;

            ItemStack stackInSlot = inventory.getItem(index);

            if (stackInSlot.isEmpty()) {
                if (mergeOnly) continue;
                inventory.setItem(index, stack.copy());
                stack.setCount(0);
            } else if (areStackable(stack, stackInSlot)) {
                int maxStackSize = Math.min(inventory.getMaxStackSize(), stackInSlot.getMaxStackSize());
                int spaceRemaining = Math.max(0, maxStackSize - stackInSlot.getCount());
                int amountToInsert = Math.min(spaceRemaining, stack.getCount());
                stack.shrink(amountToInsert);
                stackInSlot.grow(amountToInsert);
                inventory.setItem(index, stackInSlot);
            }

            if (stack.isEmpty()) break;
        }
    }

    public static boolean areStackable(ItemStack a, ItemStack b) {
        return ItemStack.isSame(a, b) && ItemStack.tagMatches(a, b) && a.getMaxStackSize() > 1 && b.getMaxStackSize() > 1;
    }
}
