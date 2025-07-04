package mrtjp.projectred.lib;

import net.minecraft.core.Direction;
import net.minecraft.core.NonNullList;
import net.minecraft.world.Container;
import net.minecraft.world.WorldlyContainer;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.items.IItemHandler;

import java.util.function.Consumer;

public class InventoryLib {

    public static void injectItemStack(Container inventory, ItemStack stack, boolean reverse) {
        injectItemStack(inventory, stack, 0, inventory.getContainerSize(), reverse);
    }

    public static void injectItemStack(Container inventory, ItemStack stack, int startIndex, int endIndex, boolean reverse) {
        injectItemStack(inventory, stack, startIndex, endIndex, reverse, true);
        if (!stack.isEmpty()) {
            injectItemStack(inventory, stack, startIndex, endIndex, reverse, false);
        }
    }

    public static boolean injectAllItemStacks(Container inventory, NonNullList<ItemStack> stacks, boolean reverse) {
        return injectAllItemStacks(inventory, stacks, 0, inventory.getContainerSize(), reverse);
    }

    public static boolean injectAllItemStacks(Container inventory, NonNullList<ItemStack> stacks, int startIndex, int endIndex, boolean reverse) {

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

    private static void injectItemStack(Container inventory, ItemStack stack, int startIndex, int endIndex, boolean reverse, boolean mergeOnly) {

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

    //region Worldly Container utilities
    public static boolean injectWorldly(WorldlyContainer container, ItemStack stack, int side, boolean simulate) {

        var insertDir = Direction.values()[side];
        var slots = container.getSlotsForFace(insertDir);

        for (int s : slots) {
            if (!container.canPlaceItemThroughFace(s, stack, insertDir)) continue;

            ItemStack stackInSlot = container.getItem(s).copy();

            if (stackInSlot.isEmpty()) {
                if (!simulate) {
                    container.setItem(s, stack.copy());
                }
                stack.setCount(0);
                return true;
            }

            if (!areStackable(stack, stackInSlot)) continue;

            int remainingSpace = Math.max(0, stackInSlot.getMaxStackSize() - stackInSlot.getCount());
            int amountToInsert = Math.min(remainingSpace, stack.getCount());
            stack.shrink(amountToInsert);
            stackInSlot.grow(amountToInsert);
            if (!simulate) {
                container.setItem(s, stackInSlot);
            }
            if (stack.isEmpty()) return true;
        }

        return false; // Entire stack was not inserted
    }

    public static boolean injectItemHandler(IItemHandler handler, ItemStack stack, boolean simulate) {
        for (int s = 0; s < handler.getSlots(); s++) {
            if (!handler.isItemValid(s, stack)) continue;

            //Note: Bug in InvWrapper.insertItem, L136. It keeps the input stack instead of copying it
            //      So we copy it here instead.
            var remainingStack = handler.insertItem(s, stack.copy(), simulate);
            if (remainingStack.isEmpty()) {
                stack.setCount(0);
                return true;
            } else {
                stack.setCount(remainingStack.getCount());
            }
        }

        return false; // Entire stack was not inserted
    }

    //endregion

    public static boolean areStackable(ItemStack a, ItemStack b) {
        return ItemStack.isSameItemSameComponents(a, b) && a.getMaxStackSize() > 1 && b.getMaxStackSize() > 1;
    }

    public static void addPlayerInventory(Inventory playerInventory, int x, int y, Consumer<Slot> slotConsumer) {
        addInventory(playerInventory, 9, x, y, 9, 3, slotConsumer); // Inventory (0 - 26)
        addInventory(playerInventory, 0, x, y + 58, 9, 1, slotConsumer); // Hotbar slots (27 - 35)
    }

    public static void addPlayerInventory(Inventory playerInventory, int x, int y, SlotFactory slotFactory, Consumer<Slot> slotConsumer) {
        addInventory(playerInventory, 9, x, y, 9, 3, slotFactory, slotConsumer); // Inventory (0 - 26)
        addInventory(playerInventory, 0, x, y + 58, 9, 1, slotFactory, slotConsumer); // Hotbar slots (27 - 35)
    }

    public static void addInventory(Container inventory, int i, int x, int y, int columns, int rows, Consumer<Slot> slotConsumer) {
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
    public static void addInventory(Container inventory, int i, int x, int y, int columns, int rows, SlotFactory slotFactory, Consumer<Slot> slotConsumer) {
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < columns; c++) {
                slotConsumer.accept(slotFactory.createSlot(inventory, i + (r * columns + c), x + c * 18, y + r * 18));
            }
        }
    }

    @FunctionalInterface
    public interface SlotFactory {
        Slot createSlot(Container inventory, int index, int x, int y);
    }
}
