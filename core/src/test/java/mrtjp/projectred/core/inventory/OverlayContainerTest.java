package mrtjp.projectred.core.inventory;

import net.minecraft.world.SimpleContainer;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class OverlayContainerTest {

    //region Read-through / copy-on-read

    @Test
    void of_readsUnderlyingItems() {
        SimpleContainer src = new SimpleContainer(2);
        src.setItem(0, new ItemStack(Items.STICK, 5));
        src.setItem(1, new ItemStack(Items.OAK_LOG, 3));

        OverlayContainer overlay = OverlayContainer.of(src);

        assertEquals(Items.STICK, overlay.getItem(0).getItem());
        assertEquals(5, overlay.getItem(0).getCount());
        assertEquals(Items.OAK_LOG, overlay.getItem(1).getItem());
        assertEquals(3, overlay.getItem(1).getCount());
    }

    @Test
    void getItem_returnsCopy_notSameReference() {
        SimpleContainer src = new SimpleContainer(1);
        ItemStack original = new ItemStack(Items.STICK, 5);
        src.setItem(0, original);

        OverlayContainer overlay = OverlayContainer.of(src);
        ItemStack fromOverlay = overlay.getItem(0);

        assertNotSame(original, fromOverlay);
        assertEquals(5, fromOverlay.getCount());
    }

    @Test
    void underlyingChange_reflectsBeforeFirstRead() {
        SimpleContainer src = new SimpleContainer(1);
        src.setItem(0, new ItemStack(Items.STICK, 5));

        OverlayContainer overlay = OverlayContainer.of(src);
        // Change underlying before any read
        src.setItem(0, new ItemStack(Items.OAK_LOG, 3));

        assertEquals(Items.OAK_LOG, overlay.getItem(0).getItem());
        assertEquals(3, overlay.getItem(0).getCount());
    }

    @Test
    void underlyingChange_doesNotReflect_afterFirstRead() {
        SimpleContainer src = new SimpleContainer(1);
        src.setItem(0, new ItemStack(Items.STICK, 5));

        OverlayContainer overlay = OverlayContainer.of(src);
        overlay.getItem(0); // trigger copy

        src.setItem(0, new ItemStack(Items.OAK_LOG, 3)); // change underlying after read

        assertEquals(Items.STICK, overlay.getItem(0).getItem());
        assertEquals(5, overlay.getItem(0).getCount());
    }

    //endregion

    //region setItem / isolation

    @Test
    void setItem_doesNotAffectUnderlying() {
        SimpleContainer src = new SimpleContainer(1);
        src.setItem(0, new ItemStack(Items.STICK, 5));

        OverlayContainer overlay = OverlayContainer.of(src);
        overlay.setItem(0, new ItemStack(Items.OAK_LOG, 10));

        assertEquals(Items.STICK, src.getItem(0).getItem());
        assertEquals(5, src.getItem(0).getCount());
    }

    @Test
    void setItem_visibleOnOverlay() {
        SimpleContainer src = new SimpleContainer(1);
        src.setItem(0, new ItemStack(Items.STICK, 5));

        OverlayContainer overlay = OverlayContainer.of(src);
        overlay.setItem(0, new ItemStack(Items.OAK_LOG, 10));

        assertEquals(Items.OAK_LOG, overlay.getItem(0).getItem());
        assertEquals(10, overlay.getItem(0).getCount());
    }

    //endregion

    //region commitChanges

    @Test
    void commitChanges_writesBackToUnderlying() {
        SimpleContainer src = new SimpleContainer(1);
        src.setItem(0, new ItemStack(Items.STICK, 5));

        OverlayContainer overlay = OverlayContainer.of(src);
        overlay.setItem(0, new ItemStack(Items.OAK_LOG, 10));
        overlay.commitChanges();

        assertEquals(Items.OAK_LOG, src.getItem(0).getItem());
        assertEquals(10, src.getItem(0).getCount());
    }

    @Test
    void commitChanges_onlyCommitsModifiedSlots() {
        SimpleContainer src = new SimpleContainer(2);
        src.setItem(0, new ItemStack(Items.STICK, 5));
        src.setItem(1, new ItemStack(Items.OAK_LOG, 3));

        OverlayContainer overlay = OverlayContainer.of(src);
        overlay.setItem(0, new ItemStack(Items.DIAMOND, 1)); // only modify slot 0
        overlay.commitChanges();

        assertEquals(Items.DIAMOND, src.getItem(0).getItem());
        // slot 1 was never accessed in overlay — should be unchanged
        assertEquals(Items.OAK_LOG, src.getItem(1).getItem());
        assertEquals(3, src.getItem(1).getCount());
    }

    //endregion

    //region clearChanges

    @Test
    void clearChanges_revertsToUnderlying() {
        SimpleContainer src = new SimpleContainer(1);
        src.setItem(0, new ItemStack(Items.STICK, 5));

        OverlayContainer overlay = OverlayContainer.of(src);
        overlay.setItem(0, new ItemStack(Items.OAK_LOG, 10));
        overlay.clearChanges();

        assertEquals(Items.STICK, overlay.getItem(0).getItem());
        assertEquals(5, overlay.getItem(0).getCount());
    }

    @Test
    void clearChanges_afterUnderlyingChange_seesNewValue() {
        SimpleContainer src = new SimpleContainer(1);
        src.setItem(0, new ItemStack(Items.STICK, 5));

        OverlayContainer overlay = OverlayContainer.of(src);
        overlay.setItem(0, new ItemStack(Items.OAK_LOG, 10)); // dirty overlay

        src.setItem(0, new ItemStack(Items.DIAMOND, 2)); // change underlying
        overlay.clearChanges(); // reset — overlay should re-read from src

        assertEquals(Items.DIAMOND, overlay.getItem(0).getItem());
        assertEquals(2, overlay.getItem(0).getCount());
    }

    //endregion

    //region Other Container methods

    @Test
    void clearContent_clearsOverlayNotUnderlying() {
        SimpleContainer src = new SimpleContainer(2);
        src.setItem(0, new ItemStack(Items.STICK, 5));
        src.setItem(1, new ItemStack(Items.OAK_LOG, 3));

        OverlayContainer overlay = OverlayContainer.of(src);
        overlay.clearContent();

        assertTrue(overlay.isEmpty());
        assertEquals(Items.STICK, src.getItem(0).getItem());
        assertEquals(Items.OAK_LOG, src.getItem(1).getItem());
    }

    @Test
    void removeItem_splitsStack_inOverlay() {
        SimpleContainer src = new SimpleContainer(1);
        src.setItem(0, new ItemStack(Items.STICK, 10));

        OverlayContainer overlay = OverlayContainer.of(src);
        ItemStack removed = overlay.removeItem(0, 3);

        assertEquals(3, removed.getCount());
        assertEquals(7, overlay.getItem(0).getCount());
        assertEquals(10, src.getItem(0).getCount()); // underlying unchanged
    }

    @Test
    void removeItemNoUpdate_clearsSlot() {
        SimpleContainer src = new SimpleContainer(1);
        src.setItem(0, new ItemStack(Items.STICK, 10));

        OverlayContainer overlay = OverlayContainer.of(src);
        ItemStack removed = overlay.removeItemNoUpdate(0);

        assertEquals(10, removed.getCount());
        assertTrue(overlay.getItem(0).isEmpty());
        assertEquals(10, src.getItem(0).getCount()); // underlying unchanged
    }

    @Test
    void isEmpty_false_whenItemsPresent() {
        SimpleContainer src = new SimpleContainer(2);
        src.setItem(0, new ItemStack(Items.STICK, 1));

        OverlayContainer overlay = OverlayContainer.of(src);

        assertFalse(overlay.isEmpty());
    }

    @Test
    void isEmpty_true_afterClearContent() {
        SimpleContainer src = new SimpleContainer(2);
        src.setItem(0, new ItemStack(Items.STICK, 5));
        src.setItem(1, new ItemStack(Items.OAK_LOG, 3));

        OverlayContainer overlay = OverlayContainer.of(src);
        overlay.clearContent();

        assertTrue(overlay.isEmpty());
    }

    @Test
    void countItem_returnsCorrectCount() {
        SimpleContainer src = new SimpleContainer(3);
        src.setItem(0, new ItemStack(Items.STICK, 10));
        src.setItem(1, new ItemStack(Items.STICK, 20));
        src.setItem(2, new ItemStack(Items.OAK_LOG, 5));

        OverlayContainer overlay = OverlayContainer.of(src);

        assertEquals(30, overlay.countItem(Items.STICK));
        assertEquals(5, overlay.countItem(Items.OAK_LOG));
    }

    //endregion

    //region Builder

    @Test
    void builder_addItemsWithRange_onlySelectedSlots() {
        SimpleContainer src = new SimpleContainer(4);
        src.setItem(0, new ItemStack(Items.STICK, 1));
        src.setItem(1, new ItemStack(Items.OAK_LOG, 2));
        src.setItem(2, new ItemStack(Items.DIAMOND, 3));
        src.setItem(3, new ItemStack(Items.GOLD_INGOT, 4));

        OverlayContainer overlay = OverlayContainer.builder()
                .addItems(src, 1, 2) // slots 1 and 2
                .build();

        assertEquals(2, overlay.getContainerSize());
        assertEquals(Items.OAK_LOG, overlay.getItem(0).getItem());
        assertEquals(Items.DIAMOND, overlay.getItem(1).getItem());
    }

    @Test
    void builder_multipleContainers_concatenates() {
        SimpleContainer src1 = new SimpleContainer(2);
        src1.setItem(0, new ItemStack(Items.STICK, 1));
        src1.setItem(1, new ItemStack(Items.OAK_LOG, 2));

        SimpleContainer src2 = new SimpleContainer(2);
        src2.setItem(0, new ItemStack(Items.DIAMOND, 3));
        src2.setItem(1, new ItemStack(Items.GOLD_INGOT, 4));

        OverlayContainer overlay = OverlayContainer.builder()
                .addItems(src1)
                .addItems(src2)
                .build();

        assertEquals(4, overlay.getContainerSize());
        assertEquals(Items.STICK, overlay.getItem(0).getItem());
        assertEquals(Items.OAK_LOG, overlay.getItem(1).getItem());
        assertEquals(Items.DIAMOND, overlay.getItem(2).getItem());
        assertEquals(Items.GOLD_INGOT, overlay.getItem(3).getItem());
    }

    @Test
    void builder_mixedMaxStackSizes_throws() {
        SimpleContainer normal = new SimpleContainer(1); // maxStackSize = 64
        SimpleContainer small = new SimpleContainer(1) {
            @Override
            public int getMaxStackSize() {
                return 16;
            }
        };

        assertThrows(RuntimeException.class, () ->
                OverlayContainer.builder()
                        .addItems(normal)
                        .addItems(small)
                        .build()
        );
    }

    @Test
    void builder_empty_defaultMaxStackSize64() {
        OverlayContainer overlay = OverlayContainer.builder().build();

        assertEquals(64, overlay.getMaxStackSize());
        assertEquals(0, overlay.getContainerSize());
    }

    //endregion

    //region newLayer

    @Test
    void newLayer_isolatesChanges() {
        SimpleContainer src = new SimpleContainer(1);
        src.setItem(0, new ItemStack(Items.STICK, 5));

        OverlayContainer layer1 = OverlayContainer.of(src);
        OverlayContainer layer2 = layer1.newLayer();

        layer2.setItem(0, new ItemStack(Items.OAK_LOG, 3));
        layer2.commitChanges();

        // layer1 now sees OAK_LOG (layer2 committed into it)
        assertEquals(Items.OAK_LOG, layer1.getItem(0).getItem());
        assertEquals(3, layer1.getItem(0).getCount());
        // underlying src is still STICK
        assertEquals(Items.STICK, src.getItem(0).getItem());
        assertEquals(5, src.getItem(0).getCount());
    }

    @Test
    void newLayer_commitBoth_writesToUnderlying() {
        SimpleContainer src = new SimpleContainer(1);
        src.setItem(0, new ItemStack(Items.STICK, 5));

        OverlayContainer layer1 = OverlayContainer.of(src);
        OverlayContainer layer2 = layer1.newLayer();

        layer2.setItem(0, new ItemStack(Items.OAK_LOG, 3));
        layer2.commitChanges();
        layer1.commitChanges();

        assertEquals(Items.OAK_LOG, src.getItem(0).getItem());
        assertEquals(3, src.getItem(0).getCount());
    }

    //endregion

    //region copyUp

    @Test
    void copyUp_isolatesFromSubsequentUnderlyingChanges() {
        SimpleContainer src = new SimpleContainer(1);
        src.setItem(0, new ItemStack(Items.STICK, 5));

        OverlayContainer overlay = OverlayContainer.of(src);
        overlay.copyUp(); // force-copy all items

        src.setItem(0, new ItemStack(Items.OAK_LOG, 3)); // change underlying

        assertEquals(Items.STICK, overlay.getItem(0).getItem());
        assertEquals(5, overlay.getItem(0).getCount());
    }

    //endregion
}
