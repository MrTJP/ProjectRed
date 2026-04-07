package mrtjp.projectred.lib;

import net.minecraft.core.NonNullList;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class InventoryLibTest {

    //region injectItemStack

    @Test
    void injectItemStack_emptyContainer_fillsSlot0() {
        SimpleContainer inv = new SimpleContainer(3);
        ItemStack stack = new ItemStack(Items.STICK, 1);
        InventoryLib.injectItemStack(inv, stack, false);

        assertEquals(1, inv.getItem(0).getCount());
        assertTrue(inv.getItem(1).isEmpty());
        assertTrue(inv.getItem(2).isEmpty());
    }

    @Test
    void injectItemStack_mergesIntoExistingPartialStack_beforeFillingEmpty() {
        SimpleContainer inv = new SimpleContainer(2);
        inv.setItem(0, new ItemStack(Items.STICK, 30));
        // slot 1 left empty

        InventoryLib.injectItemStack(inv, new ItemStack(Items.STICK, 10), false);

        assertEquals(40, inv.getItem(0).getCount());
        assertTrue(inv.getItem(1).isEmpty());
    }

    @Test
    void injectItemStack_fillsEmptyWhenNoPartial() {
        SimpleContainer inv = new SimpleContainer(2);
        inv.setItem(0, new ItemStack(Items.OAK_LOG, 1));

        InventoryLib.injectItemStack(inv, new ItemStack(Items.STICK, 1), false);

        assertEquals(Items.OAK_LOG, inv.getItem(0).getItem());
        assertEquals(Items.STICK, inv.getItem(1).getItem());
    }

    @Test
    void injectItemStack_splitsAcrossMultipleSlots() {
        SimpleContainer inv = new SimpleContainer(2);
        inv.setItem(0, new ItemStack(Items.STICK, 60));
        inv.setItem(1, new ItemStack(Items.STICK, 60));

        ItemStack stack = new ItemStack(Items.STICK, 10);
        InventoryLib.injectItemStack(inv, stack, false);

        assertEquals(64, inv.getItem(0).getCount());
        assertEquals(64, inv.getItem(1).getCount());
        assertEquals(2, stack.getCount()); // 4+4 consumed, 2 remaining
    }

    @Test
    void injectItemStack_respectsMaxStackSize() {
        SimpleContainer inv = new SimpleContainer(1);
        inv.setItem(0, new ItemStack(Items.STICK, 63));

        ItemStack stack = new ItemStack(Items.STICK, 10);
        InventoryLib.injectItemStack(inv, stack, false);

        assertEquals(64, inv.getItem(0).getCount());
        assertEquals(9, stack.getCount());
    }

    @Test
    void injectItemStack_noSpaceAvailable_inputUnchanged() {
        SimpleContainer inv = new SimpleContainer(2);
        inv.setItem(0, new ItemStack(Items.IRON_INGOT, 64));
        inv.setItem(1, new ItemStack(Items.IRON_INGOT, 64));

        ItemStack stack = new ItemStack(Items.STICK, 5);
        InventoryLib.injectItemStack(inv, stack, false);

        assertEquals(5, stack.getCount());
    }

    @Test
    void injectItemStack_reverse_fillsLastSlot() {
        SimpleContainer inv = new SimpleContainer(3);
        InventoryLib.injectItemStack(inv, new ItemStack(Items.STICK, 1), true);

        assertTrue(inv.getItem(0).isEmpty());
        assertTrue(inv.getItem(1).isEmpty());
        assertEquals(1, inv.getItem(2).getCount());
    }

    @Test
    void injectItemStack_reverse_mergesFromBack() {
        SimpleContainer inv = new SimpleContainer(2);
        inv.setItem(0, new ItemStack(Items.STICK, 30));
        inv.setItem(1, new ItemStack(Items.STICK, 30));

        InventoryLib.injectItemStack(inv, new ItemStack(Items.STICK, 10), true);

        assertEquals(30, inv.getItem(0).getCount()); // slot 0 untouched
        assertEquals(40, inv.getItem(1).getCount()); // merged into last slot first
    }

    @Test
    void injectItemStack_range_onlyTouchesSpecifiedRange() {
        SimpleContainer inv = new SimpleContainer(5);

        InventoryLib.injectItemStack(inv, new ItemStack(Items.STICK, 1), 2, 4, false);

        assertTrue(inv.getItem(0).isEmpty());
        assertTrue(inv.getItem(1).isEmpty());
        assertEquals(1, inv.getItem(2).getCount());
        assertTrue(inv.getItem(4).isEmpty());
    }

    //endregion

    //region injectAllItemStacks

    @Test
    void injectAll_allFit_returnsTrue() {
        SimpleContainer inv = new SimpleContainer(18);
        NonNullList<ItemStack> stacks = NonNullList.of(ItemStack.EMPTY,
                new ItemStack(Items.STICK, 10),
                new ItemStack(Items.OAK_LOG, 5));

        boolean result = InventoryLib.injectAllItemStacks(inv, stacks, false);

        assertTrue(result);
        assertTrue(stacks.get(0).isEmpty());
        assertTrue(stacks.get(1).isEmpty());
    }

    @Test
    void injectAll_oneCantFit_returnsFalse() {
        SimpleContainer inv = new SimpleContainer(1);
        inv.setItem(0, new ItemStack(Items.IRON_INGOT, 64));

        NonNullList<ItemStack> stacks = NonNullList.of(ItemStack.EMPTY,
                new ItemStack(Items.STICK, 1),
                new ItemStack(Items.OAK_LOG, 1));

        boolean result = InventoryLib.injectAllItemStacks(inv, stacks, false);

        assertFalse(result);
    }

    @Test
    void injectAll_emptyStackInList_skipped() {
        SimpleContainer inv = new SimpleContainer(2);
        NonNullList<ItemStack> stacks = NonNullList.of(ItemStack.EMPTY,
                ItemStack.EMPTY,
                new ItemStack(Items.STICK, 1));

        boolean result = InventoryLib.injectAllItemStacks(inv, stacks, false);

        assertTrue(result);
        assertEquals(Items.STICK, inv.getItem(0).getItem());
        assertTrue(inv.getItem(1).isEmpty());
    }

    //endregion

    //region removeItems

    @Test
    void removeItems_removesMatchingFromSingleSlot() {
        SimpleContainer inv = new SimpleContainer(1);
        inv.setItem(0, new ItemStack(Items.STICK, 5));

        int removed = InventoryLib.removeItems(inv, s -> s.is(Items.STICK), 3, false);

        assertEquals(3, removed);
        assertEquals(2, inv.getItem(0).getCount());
    }

    @Test
    void removeItems_returnsPartialWhenNotEnough() {
        SimpleContainer inv = new SimpleContainer(1);
        inv.setItem(0, new ItemStack(Items.STICK, 2));

        int removed = InventoryLib.removeItems(inv, s -> s.is(Items.STICK), 5, false);

        assertEquals(2, removed);
        assertTrue(inv.getItem(0).isEmpty());
    }

    @Test
    void removeItems_spreadsAcrossMultipleSlots() {
        SimpleContainer inv = new SimpleContainer(2);
        inv.setItem(0, new ItemStack(Items.STICK, 3));
        inv.setItem(1, new ItemStack(Items.STICK, 3));

        int removed = InventoryLib.removeItems(inv, s -> s.is(Items.STICK), 5, false);

        assertEquals(5, removed);
        assertTrue(inv.getItem(0).isEmpty());
        assertEquals(1, inv.getItem(1).getCount());
    }

    @Test
    void removeItems_predicateFilters_skipsNonMatching() {
        SimpleContainer inv = new SimpleContainer(2);
        inv.setItem(0, new ItemStack(Items.OAK_LOG, 5));
        inv.setItem(1, new ItemStack(Items.STICK, 3));

        int removed = InventoryLib.removeItems(inv, s -> s.is(Items.STICK), 10, false);

        assertEquals(3, removed);
        assertEquals(5, inv.getItem(0).getCount()); // untouched
        assertTrue(inv.getItem(1).isEmpty());
    }

    @Test
    void removeItems_reverse_removesFromLastSlotFirst() {
        SimpleContainer inv = new SimpleContainer(2);
        inv.setItem(0, new ItemStack(Items.STICK, 5));
        inv.setItem(1, new ItemStack(Items.STICK, 5));

        int removed = InventoryLib.removeItems(inv, s -> s.is(Items.STICK), 3, true);

        assertEquals(3, removed);
        assertEquals(5, inv.getItem(0).getCount()); // untouched
        assertEquals(2, inv.getItem(1).getCount()); // removed from here first
    }

    @Test
    void removeItems_range_onlyRemovesFromRange() {
        SimpleContainer inv = new SimpleContainer(5);
        for (int i = 0; i < 5; i++) inv.setItem(i, new ItemStack(Items.STICK, 5));

        InventoryLib.removeItems(inv, s -> s.is(Items.STICK), 10, 2, 4, false);

        assertEquals(5, inv.getItem(0).getCount()); // outside range
        assertEquals(5, inv.getItem(1).getCount()); // outside range
        assertTrue(inv.getItem(2).isEmpty());       // consumed
        assertTrue(inv.getItem(3).isEmpty());       // consumed
        assertEquals(5, inv.getItem(4).getCount()); // outside range
    }

    //endregion
}
