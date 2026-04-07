package mrtjp.projectred.expansion;

import net.minecraft.core.RegistryAccess;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.Container;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.crafting.*;
import net.minecraft.world.level.ItemLike;
import net.minecraft.world.level.Level;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import javax.annotation.Nullable;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class CraftingHelperScenarioTest {

    @Mock Level level;
    @Mock RecipeManager recipeManager;
    @Mock RegistryAccess registryAccess;
    @Mock Player player;

    //region Scenario model + fluent builder

    enum CraftAction { INTO_STORAGE, BY_PLAYER }

    record Scenario(
        int matrixSize,
        int storageSize,
        CraftingRecipe recipe,
        Map<Integer, ItemStack> matrixSetup,
        Map<Integer, ItemStack> storageSetup,
        boolean canConsumeFromMatrix,
        CraftAction action,
        boolean leaveRemainingInGrid,
        boolean expectedSuccess,
        ItemStack expectedTakenOutput,
        Map<Integer, ItemStack> expectedMatrixSlots,
        Map<Integer, ItemStack> expectedStorageSlots
    ) {}

    static final class Builder {
        private int matrixSize = 9;
        private int storageSize = 18;
        @Nullable private CraftingRecipe recipe;
        private final Map<Integer, ItemStack> matrixSetup = new LinkedHashMap<>();
        private final Map<Integer, ItemStack> storageSetup = new LinkedHashMap<>();
        private boolean canConsumeFromMatrix = false;
        private CraftAction action = CraftAction.INTO_STORAGE;
        private boolean leaveRemainingInGrid = false;
        private boolean expectedSuccess = true;
        private ItemStack expectedTakenOutput = ItemStack.EMPTY;
        private final Map<Integer, ItemStack> expectedMatrixSlots = new LinkedHashMap<>();
        private final Map<Integer, ItemStack> expectedStorageSlots = new LinkedHashMap<>();

        // Starting conditions and settings
        Builder recipe(CraftingRecipe recipe)      { this.recipe = recipe; return this; }
        Builder matrix(int slot, Item item)        { strictPut(matrixSetup, slot, new ItemStack(item)); return this; }
        Builder matrix(int slot, ItemStack s)      { strictPut(matrixSetup, slot, s.copy()); return this; }
        Builder storage(int slot, Item item)       { strictPut(storageSetup, slot, new ItemStack(item)); return this; }
        Builder storage(int slot, ItemStack s)     { strictPut(storageSetup, slot, s.copy()); return this; }
        /** Pack every unset storage slot with iron ingots (simulates full storage). */
        Builder fillEmptyStorage(ItemStack stack)  { for (int i = 0; i < storageSize; i++) storageSetup.computeIfAbsent(i, n -> stack.copy()); return this; }
        Builder canConsumeFromMatrix()             { canConsumeFromMatrix = true; return this; }
        Builder craftIntoStorage()                 { action = CraftAction.INTO_STORAGE; return this; }
        Builder craftByPlayer()                    { return craftByPlayer(false); }
        Builder craftByPlayer(boolean leaveInGrid) { action = CraftAction.BY_PLAYER; leaveRemainingInGrid = leaveInGrid; return this; }

        // Expectations
        Builder expectFail()                                       { expectedSuccess = false; return this; }
        Builder expectTakenOutput(ItemStack s)                     { expectedTakenOutput = s.copy(); return this; }
        Builder expectMatrixSlot(int slot, ItemLike s, int count)  { return expectMatrixSlot(slot, new ItemStack(s, count)); }
        Builder expectMatrixSlotEmpty(int slot)                    { return expectMatrixSlot(slot, ItemStack.EMPTY); }
        Builder expectMatrixSlot(int slot, ItemStack s)            { strictPut(expectedMatrixSlots, slot, s.copy()); return this; }
        Builder expectMatrixSlotRange(int a, int b, ItemStack s)   { for (int i = a; i <= b; i++) expectMatrixSlot(i, s); return this; }
        Builder expectMatrixEmpty()                                { for (int i = 0; i < matrixSize; i++) expectMatrixSlotEmpty(i); return this; }
        Builder expectStorageSlot(int slot, ItemLike s, int count) { return expectStorageSlot(slot, new ItemStack(s, count)); }
        Builder expectStorageSlotEmpty(int slot)                   { return expectStorageSlot(slot, ItemStack.EMPTY); }
        Builder expectStorageSlot(int slot, ItemStack s)           { strictPut(expectedStorageSlots, slot, s.copy()); return this; }
        Builder expectStorageSlotRange(int a, int b, ItemStack s)  { for (int i = a; i <= b; i++) expectStorageSlot(i, s); return this; }
        Builder expectStorageEmpty()                               { for (int i = 0; i < storageSize; i++) expectStorageSlotEmpty(i); return this; }

        Builder expectStorageTotal(Item item, int count)           { return this; }

        private <K, V> void strictPut(Map<K, V> dest, K key, V value) {
            assertFalse(dest.containsKey(key), "Duplicate key found: " + key);
            dest.put(key, value);
        }

        private void assertNonEmpty(Map<Integer, ItemStack> map, String message) {
            for (var entry : map.entrySet()) {
                if (!entry.getValue().isEmpty()) {
                    return;
                }
            }
            fail(message);
        }

        Scenario build() {
            assertNotNull(recipe);
            return new Scenario(matrixSize, storageSize, recipe,
                Map.copyOf(matrixSetup), Map.copyOf(storageSetup), canConsumeFromMatrix,
                action, leaveRemainingInGrid, expectedSuccess,
                    expectedTakenOutput, Map.copyOf(expectedMatrixSlots), Map.copyOf(expectedStorageSlots));
        }
    }

    static Builder scenario() { return new Builder(); }

    //endregion

    //region Recipe helpers

    static ShapedRecipe shaped(ItemStack output, Map<Character, Ingredient> key, String... pattern) {
        return new ShapedRecipe("", CraftingBookCategory.MISC, ShapedRecipePattern.of(key, pattern), output);
    }

    static ShapedRecipe shapedSingle(ItemStack output, Ingredient input) {
        return shaped(output, Map.of('A', input), "A");
    }

    //endregion

    //region Auto Crafter tests

    //region Ingredient consumption

    @Test
    void craftIntoStorage_ingredientConsumedOutputStored() {
        runScenario(scenario()
                .recipe(shapedSingle(new ItemStack(Items.OAK_PLANKS, 4), Ingredient.of(Items.OAK_LOG)))
                .matrix(0, Items.OAK_LOG)
                .storage(0, Items.OAK_LOG)
                .craftIntoStorage()
                .expectStorageSlotEmpty(0)
                .expectStorageSlot(17, Items.OAK_PLANKS, 4) // Reverse-written into storage
                .build());
    }

    @Test
    void craftIntoStorage_consumingIngredientsCreatesStorageSpace() {
        runScenario(scenario()
                .recipe(shapedSingle(new ItemStack(Items.OAK_PLANKS, 4), Ingredient.of(Items.OAK_LOG)))
                .matrix(0, Items.OAK_LOG)
                .storage(0, new ItemStack(Items.OAK_LOG, 1)) // Slot available after consuming ingredients
                .fillEmptyStorage(new ItemStack(Items.STICK))
                .craftIntoStorage()
                .expectStorageSlot(0, new ItemStack(Items.OAK_PLANKS, 4)) // Emptied slot gets item
                .expectStorageSlotRange(1, 17, new ItemStack(Items.STICK)) // Other slots unchanged
                .build());
    }

    @Test
    void craftIntoStorage_consumeFromMultipleSlots() {
        runScenario(scenario()
                .recipe(shaped(new ItemStack(Items.CRAFTING_TABLE), Map.of('P', Ingredient.of(Items.OAK_PLANKS)), "PP", "PP"))
                .matrix(4, Items.OAK_PLANKS).matrix(5, Items.OAK_PLANKS)
                .matrix(7, Items.OAK_PLANKS).matrix(8, Items.OAK_PLANKS)
                .storage(0, Items.OAK_PLANKS).storage(1, Items.OAK_PLANKS)
                .storage(2, Items.OAK_PLANKS).storage(3, Items.OAK_PLANKS)
                .craftIntoStorage()
                .expectStorageSlotEmpty(0).expectStorageSlotEmpty(1)
                .expectStorageSlotEmpty(2).expectStorageSlotEmpty(3)
                .expectStorageSlot(17, Items.CRAFTING_TABLE, 1)
                .build());
    }

    @Test
    void craftIntoStorage_consumeFromSingleSlot() {
        runScenario(scenario()
                .recipe(shaped(new ItemStack(Items.CRAFTING_TABLE), Map.of('P', Ingredient.of(Items.OAK_PLANKS)), "PP", "PP"))
                .matrix(4, Items.OAK_PLANKS).matrix(5, Items.OAK_PLANKS)
                .matrix(7, Items.OAK_PLANKS).matrix(8, Items.OAK_PLANKS)
                .storage(0, new ItemStack(Items.OAK_PLANKS, 4)) // All ingredients in 1 slot
                .craftIntoStorage()
                .expectStorageSlotEmpty(0)
                .expectStorageSlot(17, Items.CRAFTING_TABLE, 1)
                .build());
    }

    @Test
    void craftIntoStorage_failsWhenIngredientMissing() {
        runScenario(scenario()
                .recipe(shapedSingle(new ItemStack(Items.OAK_PLANKS, 4), Ingredient.of(Items.OAK_LOG)))
                .matrix(0, Items.OAK_LOG)
                // storage intentionally empty
                .craftIntoStorage()
                .expectFail()
                .build());
    }

    @Test
    void craftIntoStorage_failsWhenStorageFull() {
        runScenario(scenario()
                .recipe(shapedSingle(new ItemStack(Items.OAK_PLANKS, 4), Ingredient.of(Items.OAK_LOG)))
                .matrix(0, Items.OAK_LOG)
                .storage(0, new ItemStack(Items.OAK_LOG, 2)) // Too many to consume and make empty slot
                .fillEmptyStorage(new ItemStack(Items.STICK))
                .craftIntoStorage()
                .expectFail()
                .build());
    }

    //endregion

    //region Matrix consumption

    @Test
    void craftIntoStorage_consumesFromMatrixWhenEnabled() {
        runScenario(scenario()
                .recipe(shapedSingle(new ItemStack(Items.OAK_PLANKS, 4), Ingredient.of(Items.OAK_LOG)))
                .matrix(0, Items.OAK_LOG)
                // storage empty — ingredient only in matrix
                .canConsumeFromMatrix()
                .craftIntoStorage()
                .expectMatrixEmpty()
                .expectStorageSlotEmpty(0)
                .expectStorageSlot(17, Items.OAK_PLANKS, 4)
                .build());
    }

    //endregion

    //region Remaining items

    @Test
    void craftIntoStorage_remainingItemStoredAlongsideOutput() {
        runScenario(scenario()
                .recipe(shapedSingle(new ItemStack(Items.CAKE), Ingredient.of(Items.WATER_BUCKET)))
                .matrix(0, Items.WATER_BUCKET)
                .storage(0, Items.WATER_BUCKET)
                .craftIntoStorage()
                .expectStorageSlotEmpty(0)
                .expectStorageSlot(17, Items.CAKE, 1)
                .expectStorageSlot(16, Items.BUCKET, 1)
                .build());
    }

    //endregion

    //endregion

    //region Project Bench tests

    //region Ingredient consumption

    //CASE: Some ingredients in storage but some in matrix

    @Test
    void craftByPlayer_ingredientConsumedOnSuccess() {
        runScenario(scenario()
                .recipe(shapedSingle(new ItemStack(Items.OAK_PLANKS, 4), Ingredient.of(Items.OAK_LOG)))
                .matrix(0, Items.OAK_LOG)
                .storage(0, Items.OAK_LOG)
                .craftByPlayer()
                .expectTakenOutput(new ItemStack(Items.OAK_PLANKS, 4))
                .expectStorageEmpty()
                .build());
    }

    //endregion

    //region Remaining items

    @Test
    void craftByPlayer_remainingSentToStorageWhenLeaveRemainingFalse() {
        runScenario(scenario()
                .recipe(shapedSingle(new ItemStack(Items.CAKE), Ingredient.of(Items.WATER_BUCKET)))
                .matrix(0, Items.WATER_BUCKET)
                .storage(0, Items.WATER_BUCKET)
                .craftByPlayer(false)
                .expectStorageSlotEmpty(0)
                .expectStorageSlot(17, Items.BUCKET, 1)
                .expectTakenOutput(new ItemStack(Items.CAKE, 1))
                .build());
    }

    @Test
    void craftByPlayer_remainingLeftInMatrixWhenConsumingFromMatrix() {
        runScenario(scenario()
                .recipe(shapedSingle(new ItemStack(Items.CAKE), Ingredient.of(Items.WATER_BUCKET)))
                .matrix(0, Items.WATER_BUCKET)
                // storage empty; consume from matrix so slot 0 is freed for the remaining bucket
                .canConsumeFromMatrix()
                .craftByPlayer(true)
                .expectMatrixSlot(0, new ItemStack(Items.BUCKET))
                .expectStorageEmpty()
                .expectTakenOutput(new ItemStack(Items.CAKE, 1))
                .build());
    }

    @Test
    void craftByPlayer_remainingNotLeftInMatrixWhenNotConsumingFromMatrix() {
        runScenario(scenario()
                .recipe(shapedSingle(new ItemStack(Items.CAKE), Ingredient.of(Items.WATER_BUCKET)))
                .matrix(0, Items.WATER_BUCKET)
                .storage(0, Items.WATER_BUCKET)
                .craftByPlayer(true) // set leave in matrix
                .expectStorageSlotEmpty(0)
                .expectStorageSlot(17, Items.BUCKET, 1) // Matrix still has original bucket, so falls back here
                .expectTakenOutput(new ItemStack(Items.CAKE, 1))
                .build());
    }

    @Test
    void craftByPlayer_remainingFallsBackToPlayerWhenStorageFull() {
        runScenario(scenario()
                .recipe(shapedSingle(new ItemStack(Items.CAKE), Ingredient.of(Items.WATER_BUCKET)))
                .matrix(0, Items.WATER_BUCKET)
                .canConsumeFromMatrix()
                .fillEmptyStorage(new ItemStack(Items.STICK))
                .craftByPlayer(false)
                .expectMatrixEmpty()
                .expectTakenOutput(new ItemStack(Items.CAKE, 1))
                .build());

        //TODO test this thru scenario
        verify(player).addItem(argThat(s -> s.getItem() == Items.BUCKET));
    }

    //endregion

    //endregion

    //region Scenario runner

    private void runScenario(Scenario s) {
        // Set up containers
        SimpleContainer matrix = new SimpleContainer(s.matrixSize());
        SimpleContainer storage = new SimpleContainer(s.storageSize());
        s.matrixSetup().forEach((slot, stack) -> matrix.setItem(slot, stack.copy()));
        s.storageSetup().forEach((slot, stack) -> storage.setItem(slot, stack.copy()));

        // Wire up mocks; recipeManager delegates matching to the recipe
        RecipeHolder<CraftingRecipe> recipeHolder = new RecipeHolder<>(
                ResourceLocation.parse("test:recipe"), s.recipe());

        lenient().when(level.getRecipeManager()).thenReturn(recipeManager);
        lenient().when(level.registryAccess()).thenReturn(registryAccess);
        lenient().when(recipeManager.getRecipeFor(eq(RecipeType.CRAFTING), any(CraftingInput.class), any()))
                .thenAnswer(inv -> s.recipe().matches(inv.getArgument(1), level)
                        ? Optional.of(recipeHolder) : Optional.empty());

        CraftingHelper helper = new CraftingHelper(new CraftingHelper.InventorySource() {
            @Override public Container getCraftingMatrix()          { return matrix; }
            @Override public Container getStorage()                 { return storage; }
            @Override public boolean canConsumeFromCraftingMatrix() { return s.canConsumeFromMatrix(); }
            @Override public Level getWorld()                       { return level; }
        });
        helper.onInventoryChanged();

        boolean testResult = false;
        boolean result = false;

        switch (s.action) {
            case INTO_STORAGE -> {
                testResult = helper.canTakeIntoStorage();
                result = helper.onCraftedIntoStorage();
            }
            case BY_PLAYER -> {
                testResult = helper.canTake();
                ItemStack taken = helper.getRecipeOutput();
                result = helper.onCraftedByPlayer(player, s.leaveRemainingInGrid());

                assertTrue(ItemStack.matches(taken, s.expectedTakenOutput()), "Player taken crafting output does not match expected");
            }
        }

        assertEquals(testResult, result, "Craft pre-check result does not match final result");
        assertEquals(s.expectedSuccess(), result, "craft action return value");
        assertContainerChanges(matrix, s.matrixSetup(), s.expectedMatrixSlots(), "Invalid matrix changes");
        assertContainerChanges(storage, s.storageSetup(), s.expectedStorageSlots(), "Invalid storage changes");
    }

    //endregion

    //region Helpers

    private static void assertContainerContents(Container container, Map<Integer, ItemStack> map, String containerName) {
        for (int i = 0; i < container.getContainerSize(); i++) {
            ItemStack actual = container.getItem(i);
            ItemStack expected = map.getOrDefault(i, ItemStack.EMPTY);
            assertTrue(ItemStack.matches(expected, actual),
                    containerName + "[" + i + "] expected " + expected + ", got " + actual);
        }
    }

    private static void assertContainerChanges(Container container, Map<Integer, ItemStack> initial, Map<Integer, ItemStack> expectedChanges, String message) {
        // All slots should not change unless explicitly expected
        for (int i = 0; i < container.getContainerSize(); i++) {
            ItemStack actual = container.getItem(i);
            ItemStack expected = expectedChanges.getOrDefault(i, initial.getOrDefault(i, ItemStack.EMPTY));
            String expectation = expectedChanges.containsKey(i) ? "change to ": "remain ";

            assertTrue(ItemStack.matches(expected, actual),
                    message + ": Slot [" + i + "] expected to " + expectation + expected + ", but got " + actual);
        }
    }

    //endregion
}
