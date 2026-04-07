package mrtjp.projectred.expansion;

import net.minecraft.core.NonNullList;
import net.minecraft.core.RegistryAccess;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.Container;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.crafting.CraftingInput;
import net.minecraft.world.item.crafting.CraftingRecipe;
import net.minecraft.world.item.crafting.RecipeHolder;
import net.minecraft.world.item.crafting.RecipeManager;
import net.minecraft.world.item.crafting.RecipeType;
import net.minecraft.world.level.Level;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
public class CraftingHelperTest {

    SimpleContainer matrix;   // 9 slots — recipe pattern
    SimpleContainer storage;  // 18 slots — ingredient source

    @Mock Level level;
    @Mock RecipeManager recipeManager;
    @Mock RegistryAccess registryAccess;
    @Mock Player player;
    @Mock CraftingRecipe mockRecipe;

    RecipeHolder<CraftingRecipe> recipeHolder;
    CraftingHelper helper;

    @BeforeEach
    void setUp() {
        matrix  = new SimpleContainer(9);
        storage = new SimpleContainer(18);

        recipeHolder = new RecipeHolder<>(ResourceLocation.parse("test:recipe"), mockRecipe);

        lenient().when(level.getRecipeManager()).thenReturn(recipeManager);
        lenient().when(level.registryAccess()).thenReturn(registryAccess);
        lenient().when(recipeManager.getRecipeFor(eq(RecipeType.CRAFTING), any(CraftingInput.class), any()))
                .thenReturn(Optional.of(recipeHolder));
        lenient().when(mockRecipe.matches(any(CraftingInput.class), any(Level.class))).thenReturn(true);
        lenient().when(mockRecipe.assemble(any(CraftingInput.class), any())).thenReturn(new ItemStack(Items.STICK, 4));
        lenient().when(mockRecipe.getRemainingItems(any(CraftingInput.class))).thenReturn(NonNullList.withSize(9, ItemStack.EMPTY));

        helper = makeHelper(false);
    }

    // Build a helper with configurable canConsumeFromCraftingMatrix
    private CraftingHelper makeHelper(boolean consumeFromMatrix) {
        return new CraftingHelper(new CraftingHelper.InventorySource() {
            @Override public Container getCraftingMatrix() { return matrix; }
            @Override public Container getStorage()        { return storage; }
            @Override public boolean canConsumeFromCraftingMatrix() { return consumeFromMatrix; }
            @Override public Level getWorld()              { return level; }
        });
    }

    // Fill all 9 matrix slots and corresponding storage slots with 1 stick each
    private void setUpFullMatrixAndStorageWithSticks() {
        for (int i = 0; i < 9; i++) {
            matrix.setItem(i, new ItemStack(Items.STICK));
            storage.setItem(i, new ItemStack(Items.STICK));
        }
    }

    //region clear()

    @Test
    public void testClear_resetsAllState() {
        setUpFullMatrixAndStorageWithSticks();
        helper.onInventoryChanged();
        helper.clear();

        assertFalse(helper.hasRecipe());
        assertTrue(helper.getRecipeOutput().isEmpty());
        assertFalse(helper.canTake());
        assertFalse(helper.canTakeIntoStorage());
        assertEquals(0, helper.getMissingIngredientMask());
        assertTrue(helper.getCraftingInventory().isEmpty());
        assertTrue(helper.getCraftResultInventory().isEmpty());
    }

    //endregion

    //region onInventoryChanged()

    @Test
    public void testOnInventoryChanged_copiesMatrixToInternalInventory() {
        matrix.setItem(3, new ItemStack(Items.STICK));
        helper.onInventoryChanged();

        assertEquals(Items.STICK, helper.getCraftingInventory().getItem(3).getItem());
        assertTrue(helper.getCraftingInventory().getItem(0).isEmpty());
    }

    @Test
    public void testOnInventoryChanged_noRecipe_stateIsClean() {
        when(recipeManager.getRecipeFor(any(), any(CraftingInput.class), any()))
                .thenReturn(Optional.empty());
        matrix.setItem(0, new ItemStack(Items.STICK));
        helper.onInventoryChanged();

        assertFalse(helper.hasRecipe());
        assertTrue(helper.getRecipeOutput().isEmpty());
        assertFalse(helper.canTake());
        assertFalse(helper.canTakeIntoStorage());
    }

    @Test
    public void testOnInventoryChanged_withRecipe_setsHasRecipeAndOutput() {
        matrix.setItem(0, new ItemStack(Items.STICK));
        storage.setItem(0, new ItemStack(Items.STICK));
        helper.onInventoryChanged();

        assertTrue(helper.hasRecipe());
        assertEquals(Items.STICK, helper.getRecipeOutput().getItem());
    }

    //endregion

    //region canTake()

    @Test
    public void testCanTake_trueWhenIngredientInStorage() {
        matrix.setItem(0, new ItemStack(Items.STICK));
        storage.setItem(0, new ItemStack(Items.STICK));
        helper.onInventoryChanged();

        assertTrue(helper.canTake());
    }

    @Test
    public void testCanTake_falseWhenStorageEmpty() {
        matrix.setItem(0, new ItemStack(Items.STICK));
        // storage left empty
        helper.onInventoryChanged();

        assertFalse(helper.canTake());
    }

    @Test
    public void testCanTake_withMatrixConsumption_usesMatrixItems() {
        helper = makeHelper(true);
        matrix.setItem(0, new ItemStack(Items.STICK));
        // storage is EMPTY — ingredient only in matrix
        helper.onInventoryChanged();

        assertTrue(helper.canTake());
    }

    @Test
    public void testCanTake_withoutMatrixConsumption_ignoresMatrixItems() {
        // default helper — canConsumeFromCraftingMatrix=false
        matrix.setItem(0, new ItemStack(Items.STICK));
        // storage is EMPTY
        helper.onInventoryChanged();

        assertFalse(helper.canTake());
    }

    //endregion

    //region getMissingIngredientMask()

    @Test
    public void testMissingIngredientMask_zeroWhenAllPresent() {
        setUpFullMatrixAndStorageWithSticks();
        helper.onInventoryChanged();

        assertEquals(0, helper.getMissingIngredientMask());
    }

    @Test
    public void testMissingIngredientMask_bit0WhenSlot0Missing() {
        matrix.setItem(0, new ItemStack(Items.STICK));
        // storage empty
        helper.onInventoryChanged();

        assertEquals(0b000000001, helper.getMissingIngredientMask());
    }

    @Test
    public void testMissingIngredientMask_multipleBitsForMultipleMissing() {
        matrix.setItem(0, new ItemStack(Items.STICK));
        matrix.setItem(1, new ItemStack(Items.STICK));
        matrix.setItem(2, new ItemStack(Items.STICK));
        // storage empty
        helper.onInventoryChanged();

        assertEquals(0b000000111, helper.getMissingIngredientMask());
    }

    //endregion

    //region canTakeIntoStorage()

    @Test
    public void testCanTakeIntoStorage_trueWhenStorageHasRoom() {
        matrix.setItem(0, new ItemStack(Items.STICK));
        storage.setItem(0, new ItemStack(Items.STICK)); // ingredient; slots 1-17 free
        helper.onInventoryChanged();

        assertTrue(helper.canTakeIntoStorage());
    }

    @Test
    public void testCanTakeIntoStorage_falseWhenStorageFull() {
        // Output = diamond sword (max stack 1) so it can't merge anywhere
        when(mockRecipe.assemble(any(), any())).thenReturn(new ItemStack(Items.DIAMOND_SWORD));

        matrix.setItem(0, new ItemStack(Items.STICK));
        // Slot 0 has 2 sticks: consuming 1 leaves 1 stick (slot NOT empty)
        storage.setItem(0, new ItemStack(Items.STICK, 2));
        // All other 17 slots packed full
        for (int i = 1; i < 18; i++) {
            storage.setItem(i, new ItemStack(Items.IRON_INGOT, 64));
        }
        helper.onInventoryChanged();

        // No room for the sword — every slot is occupied and it can't stack with sticks/ingots
        assertFalse(helper.canTakeIntoStorage());
    }

    //endregion

    //region onCraftedByPlayer()

    @Test
    public void testOnCraftedByPlayer_returnsFalseWithNoRecipe() {
        when(recipeManager.getRecipeFor(any(), any(CraftingInput.class), any()))
                .thenReturn(Optional.empty());
        matrix.setItem(0, new ItemStack(Items.STICK));
        storage.setItem(0, new ItemStack(Items.STICK));
        helper.onInventoryChanged();

        assertFalse(helper.onCraftedByPlayer(player, false));
        assertEquals(Items.STICK, storage.getItem(0).getItem()); // untouched
    }

    @Test
    public void testOnCraftedByPlayer_consumesIngredientFromStorage() {
        matrix.setItem(0, new ItemStack(Items.STICK));
        storage.setItem(0, new ItemStack(Items.STICK));
        helper.onInventoryChanged();

        assertTrue(helper.onCraftedByPlayer(player, false));
        assertTrue(storage.getItem(0).isEmpty());
    }

    @Test
    public void testOnCraftedByPlayer_remainingGoesToGrid_whenMatrixConsumed() {
        helper = makeHelper(true); // matrix items are also consumed

        // Remaining: a bucket appears at crafting-input slot 0
        NonNullList<ItemStack> remaining = NonNullList.withSize(9, ItemStack.EMPTY);
        remaining.set(0, new ItemStack(Items.BUCKET));
        when(mockRecipe.getRemainingItems(any())).thenReturn(remaining);

        // Full 3x3 matrix of sticks so CraftingInput is 3x3 at (left=0, top=0) — slot 0 maps to matrix slot 0
        for (int i = 0; i < 9; i++) matrix.setItem(i, new ItemStack(Items.STICK));
        // Storage empty; ingredients consumed from matrix via overlay
        helper.onInventoryChanged();

        assertTrue(helper.onCraftedByPlayer(player, true));

        // After consuming the stick from matrix slot 0, the slot is empty.
        // The bucket remaining item is then placed back into slot 0.
        assertEquals(Items.BUCKET, matrix.getItem(0).getItem());
    }

    @Test
    public void testOnCraftedByPlayer_remainingGoesToStorage_whenLeaveRemainingFalse() {
        NonNullList<ItemStack> remaining = NonNullList.withSize(9, ItemStack.EMPTY);
        remaining.set(0, new ItemStack(Items.BUCKET));
        when(mockRecipe.getRemainingItems(any())).thenReturn(remaining);

        matrix.setItem(0, new ItemStack(Items.STICK));
        storage.setItem(0, new ItemStack(Items.STICK));
        helper.onInventoryChanged();
        helper.onCraftedByPlayer(player, false);

        boolean bucketInStorage = false;
        for (int i = 0; i < 18; i++) {
            if (storage.getItem(i).getItem() == Items.BUCKET) { bucketInStorage = true; break; }
        }
        assertTrue(bucketInStorage);
    }

    @Test
    public void testOnCraftedByPlayer_remainingFallsBackToPlayer_whenStorageFull() {
        NonNullList<ItemStack> remaining = NonNullList.withSize(9, ItemStack.EMPTY);
        remaining.set(0, new ItemStack(Items.DIAMOND_SWORD)); // can't merge into anything
        when(mockRecipe.getRemainingItems(any())).thenReturn(remaining);

        matrix.setItem(0, new ItemStack(Items.STICK));
        storage.setItem(0, new ItemStack(Items.STICK, 2)); // consume 1 → 1 left, slot not empty
        for (int i = 1; i < 18; i++) storage.setItem(i, new ItemStack(Items.IRON_INGOT, 64));

        helper.onInventoryChanged();
        helper.onCraftedByPlayer(player, false);

        verify(player).addItem(argThat(s -> s.getItem() == Items.DIAMOND_SWORD));
    }

    @Test
    public void testOnCraftedByPlayer_sourceUnalteredOnFailure() {
        // Recipe found, but matches() = false → craftFromSource returns EMPTY
        when(mockRecipe.matches(any(), any())).thenReturn(false);

        matrix.setItem(0, new ItemStack(Items.STICK));
        storage.setItem(0, new ItemStack(Items.STICK));
        helper.onInventoryChanged();

        assertFalse(helper.onCraftedByPlayer(player, false));
        assertEquals(1, storage.getItem(0).getCount()); // overlay never committed
    }

    //endregion

    //region onCraftedIntoStorage()

    @Test
    public void testOnCraftedIntoStorage_storesOutputInStorage() {
        matrix.setItem(0, new ItemStack(Items.STICK));
        storage.setItem(0, new ItemStack(Items.STICK)); // ingredient; slots 1-17 free for output
        helper.onInventoryChanged();

        assertTrue(helper.onCraftedIntoStorage());

        // 4 sticks output injected; original 1-stick ingredient consumed → net = 4 sticks
        int totalSticks = 0;
        for (int i = 0; i < 18; i++) {
            if (storage.getItem(i).getItem() == Items.STICK) {
                totalSticks += storage.getItem(i).getCount();
            }
        }
        assertEquals(4, totalSticks);
    }

    @Test
    public void testOnCraftedIntoStorage_storesRemainingItemsInStorage() {
        NonNullList<ItemStack> remaining = NonNullList.withSize(9, ItemStack.EMPTY);
        remaining.set(0, new ItemStack(Items.BUCKET));
        when(mockRecipe.getRemainingItems(any())).thenReturn(remaining);

        matrix.setItem(0, new ItemStack(Items.STICK));
        storage.setItem(0, new ItemStack(Items.STICK));
        helper.onInventoryChanged();

        assertTrue(helper.onCraftedIntoStorage());

        boolean bucketInStorage = false;
        for (int i = 0; i < 18; i++) {
            if (storage.getItem(i).getItem() == Items.BUCKET) { bucketInStorage = true; break; }
        }
        assertTrue(bucketInStorage);
    }

    @Test
    public void testOnCraftedIntoStorage_returnsFalseWhenNoRoom() {
        when(mockRecipe.assemble(any(), any())).thenReturn(new ItemStack(Items.DIAMOND_SWORD));

        matrix.setItem(0, new ItemStack(Items.STICK));
        storage.setItem(0, new ItemStack(Items.STICK, 2));
        for (int i = 1; i < 18; i++) storage.setItem(i, new ItemStack(Items.IRON_INGOT, 64));
        helper.onInventoryChanged();

        assertFalse(helper.onCraftedIntoStorage());
    }

    @Test
    public void testOnCraftedIntoStorage_sourceUnalteredOnFailure() {
        when(mockRecipe.assemble(any(), any())).thenReturn(new ItemStack(Items.DIAMOND_SWORD));

        matrix.setItem(0, new ItemStack(Items.STICK));
        storage.setItem(0, new ItemStack(Items.STICK, 2));
        for (int i = 1; i < 18; i++) storage.setItem(i, new ItemStack(Items.IRON_INGOT, 64));
        helper.onInventoryChanged();

        int[] beforeCounts = new int[18];
        for (int i = 0; i < 18; i++) beforeCounts[i] = storage.getItem(i).getCount();

        helper.onCraftedIntoStorage();

        for (int i = 0; i < 18; i++) {
            assertEquals(beforeCounts[i], storage.getItem(i).getCount(),
                    "storage slot " + i + " was unexpectedly modified");
        }
    }

    //endregion
}
