package mrtjp.projectred.lib;

import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.util.NonNullList;

import java.util.function.Consumer;

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

    public static void addPlayerInventory(PlayerInventory playerInventory, int x, int y, Consumer<Slot> slotConsumer) {
        addInventory(playerInventory, 9, x, y, 9, 3, slotConsumer); // Inventory (0 - 26)
        addInventory(playerInventory, 0, x, y + 58, 9, 1, slotConsumer); // Hotbar slots (27 - 35)
    }

    public static void addPlayerInventory(PlayerInventory playerInventory, int x, int y, SlotFactory slotFactory, Consumer<Slot> slotConsumer) {
        addInventory(playerInventory, 9, x, y, 9, 3, slotFactory, slotConsumer); // Inventory (0 - 26)
        addInventory(playerInventory, 0, x, y + 58, 9, 1, slotFactory, slotConsumer); // Hotbar slots (27 - 35)
    }

    public static void addInventory(IInventory inventory, int i, int x, int y, int columns, int rows, Consumer<Slot> slotConsumer) {
        addInventory(inventory, i, x, y, columns, rows, Slot::new, slotConsumer);
    }

    /**
     * Creates a grid of slots for a container with standard spacing.
     *
     * @param inventory The inventory backing the slots
     * @param i Inventory index of the first slot
     * @param x X position of the top left slot
     * @param y Y position of the top left slot
     * @param columns Number of columns in the grid
     * @param rows Number of rows in the grid
     * @param slotFactory Factory for creating the slots
     * @param slotConsumer Consumer for the slots (typically Container.addSlot(...))
     */
    public static void addInventory(IInventory inventory, int i, int x, int y, int columns, int rows, SlotFactory slotFactory, Consumer<Slot> slotConsumer) {
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < columns; c++) {
                slotConsumer.accept(slotFactory.createSlot(inventory, i + (r * columns + c), x + c * 18, y + r * 18));
            }
        }
    }

    @FunctionalInterface
    public interface SlotFactory {
        Slot createSlot(IInventory inventory, int index, int x, int y);
    }
}
