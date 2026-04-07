package mrtjp.projectred.expansion;

import mrtjp.projectred.core.inventory.BaseContainer;
import mrtjp.projectred.core.inventory.OverlayContainer;
import mrtjp.projectred.lib.InventoryLib;
import net.minecraft.core.NonNullList;
import net.minecraft.world.Container;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.CraftingContainer;
import net.minecraft.world.inventory.ResultContainer;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.CraftingInput;
import net.minecraft.world.item.crafting.CraftingRecipe;
import net.minecraft.world.item.crafting.RecipeHolder;
import net.minecraft.world.item.crafting.RecipeType;
import net.minecraft.world.level.Level;
import net.neoforged.neoforge.common.CommonHooks;

import javax.annotation.Nullable;

public class CraftingHelper {

    private static class CraftingHelperContainer extends BaseContainer implements CraftingContainer {
        public CraftingHelperContainer() {
            super(9);
        }

        @Override
        public int getWidth() {
            return 3;
        }

        @Override
        public int getHeight() {
            return 3;
        }

        @Override
        public NonNullList<ItemStack> getItems() {
            return NonNullList.copyOf(items);
        }
    }

    private final CraftingContainer craftingInventory = new CraftingHelperContainer();

    private final ResultContainer craftResultInventory = new ResultContainer();

    private final InventorySource inputSource;

    private @Nullable RecipeHolder<CraftingRecipe> recipe = null;
    private CraftingInput.Positioned posCraftingInput = CraftingInput.Positioned.EMPTY;
    private CraftingResult result = CraftingResult.EMPTY;
    private boolean canFitResultsIntoSource = false;

    public CraftingHelper(InventorySource inputSource) {
        this.inputSource = inputSource;
    }

    //region Inventory events

    /**
     * Clears internal state such as located recipe, crafting result, etc.
     */
    public void clear() {
        craftingInventory.clearContent();
        craftResultInventory.clearContent();
        recipe = null;
        posCraftingInput = CraftingInput.Positioned.EMPTY;
        result = CraftingResult.EMPTY;
        canFitResultsIntoSource = false;
    }

    /**
     * Refreshes recipe from crafting matrix and re-calculates feasibility of
     * crafting the recipe output.
     */
    public void onInventoryChanged() {
        loadInputs();
        loadRecipe();
        loadOutput();
    }
    //endregion

    //region Container getters
    public CraftingContainer getCraftingInventory() {
        return craftingInventory;
    }

    public ResultContainer getCraftResultInventory() {
        return craftResultInventory;
    }
    //region

    private void loadInputs() {
        Container craftingMatrix = inputSource.getCraftingMatrix();
        // Copy recipe matrix to internal Crafting Inventory
        for (int i = 0; i < 9; i++) {
            craftingInventory.setItem(i, craftingMatrix.getItem(i).copy());
        }
        posCraftingInput = craftingInventory.asPositionedCraftInput();
    }

    private void loadRecipe() {
        recipe = inputSource.getWorld().getRecipeManager()
                .getRecipeFor(RecipeType.CRAFTING, posCraftingInput.input(), inputSource.getWorld()).orElse(null);

        craftResultInventory.setItem(0, recipe == null ? ItemStack.EMPTY : recipe.value().assemble(posCraftingInput.input(), inputSource.getWorld().registryAccess()));
    }

    private void loadOutput() {
        OverlayContainer overlay = createAvailableStorageOverlay();
        result = craftFromSource(overlay, null);

        if (result.isCraftable()) {
            NonNullList<ItemStack> allResults = result.getOutputAndRemaining();
            canFitResultsIntoSource = InventoryLib.injectAllItemStacks(overlay, allResults, true);
        }
    }

    //region Public interface

    /**
     * Check if crafting matrix holds valid recipe ingredients.
     * <p>
     * Refreshed on {@link #onInventoryChanged()}
     *
     * @return True if matrix matches recipe
     */
    public boolean hasRecipe() {
        return recipe != null;
    }

    /**
     * Returns output item of the current recipe in the crafting matrix
     * <p>
     * Refreshed on {@link #onInventoryChanged()}
     *
     * @return The recipe output item
     */
    public ItemStack getRecipeOutput() {
        return craftResultInventory.getItem(0);
    }

    /**
     * Checks if the crafting ingredient sources contains enough ingredients to craft the current recipe.
     * <p>
     * Refreshed on {@link #onInventoryChanged()}
     *
     * @return True if crafting is possible
     */
    public boolean canTake() {
        return result.isCraftable();
    }

    /**
     * Checks if sources contain the necessary ingredients to craft, and then also has the space to
     * take in the results and remaining items post-craft.
     * <p>
     * Refreshed on {@link #onInventoryChanged()}
     *
     * @return True if crafting and storing is possible
     */
    public boolean canTakeIntoStorage() {
        return canTake() && canFitResultsIntoSource;
    }

    /**
     * A 9-bit mask representing slots of the 3x3 matrix. Bits are high if ingredient is missing.
     * <p>
     * Refreshed on {@link #onInventoryChanged()}
     *
     * @return Missing ingredient mask
     */
    public int getMissingIngredientMask() {
        return result.missingIngredientMask;
    }

    /**
     * Executes a player-based craft, typically from an output slot's onTake() method. This will consume ingredients from the source
     * containers.
     * <p>
     * Contract:
     * - Will succeed and return true if canTake() is true
     * - Source containers left unaltered on failure
     *
     * @param player               The crafting player
     * @param leaveRemainingInGrid If remaining items should be left in grid. False returns them to storage.
     * @return True if crafting was successful (ingredients consumed, remaining stored or dropped)
     */
    public boolean onCraftedByPlayer(Player player, boolean leaveRemainingInGrid) {
        if (recipe == null) return false;

        // Attempt to consume ingredients and craft
        OverlayContainer overlay = createAvailableStorageOverlay();
        CraftingResult result = craftFromSource(overlay, player);
        if (!result.isCraftable()) return false;

        // Crafting successful. Finalize removal of ingredients
        overlay.commitChanges();

        // Put remaining items back
        Container craftingGird = inputSource.getCraftingMatrix();
        Container storage = inputSource.getStorage();

        for (int i = 0; i < result.getRemainingItems().size(); i++) {
            ItemStack remaining = result.getRemainingItems().get(i);
            if (remaining.isEmpty()) continue;

            // If allowed, leave remaining in crafting grid just like Vanilla crafting bench
            int ccSlot = craftingInputSlotToContainer(craftingInventory, posCraftingInput, i);
            if (leaveRemainingInGrid && craftingGird.getItem(ccSlot).isEmpty()) {
                craftingGird.setItem(ccSlot, remaining.split(remaining.getCount()));
                continue;
            }

            // Otherwise try to put it somewhere
            InventoryLib.injectItemStack(storage, remaining, true); // In storage
            if (!remaining.isEmpty()) player.addItem(remaining);    // In player inventory
            if (!remaining.isEmpty()) player.drop(remaining, false);// Or as last resort, on ground
        }

        return true;
    }

    /**
     * Crafts the recipe and puts result and all remaining items back into storage container.
     * <p>
     * Contracts:
     * - Will succeed and return true if canTakeIntoStorage is true
     * - Source containers left unaltered on failure
     * - Items can be consumed from matrix if enabled, but result and remaining items will NEVER go back to matrix
     *
     * @return True if successful
     */
    public boolean onCraftedIntoStorage() {
        // Create overlay and attempt to consume ingredients
        OverlayContainer overlay = createAvailableStorageOverlay();
        CraftingResult result = craftFromSource(overlay, null);
        if (!result.isCraftable()) return false;

        // Try to store result items back into storage after ingredients are consumed
        NonNullList<ItemStack> allResults = result.getOutputAndRemaining();
        // Note: This directly assumes first X slots are storage (See createAvailableStorageOverlay)
        int storageSize = inputSource.getStorage().getContainerSize();
        boolean fits = InventoryLib.injectAllItemStacks(overlay, allResults, 0, storageSize, true);

        // Commit if everything fits
        if (fits) {
            overlay.commitChanges();
            return true;
        }

        return false;
    }
    //endregion

    //region Utils
    private CraftingResult craftFromSource(Container source, @Nullable Player player) {
        if (recipe == null) return CraftingResult.EMPTY;

        if (!recipe.value().matches(posCraftingInput.input(), inputSource.getWorld())) return CraftingResult.EMPTY;

        ItemStack result = recipe.value().assemble(posCraftingInput.input(), inputSource.getWorld().registryAccess());
        if (result.isEmpty()) return CraftingResult.EMPTY;

        // Try to consume all ingredients
        int missingIngredientMask = 0;
        for (int i = 0; i < 9; i++) {
            final int slot = i;
            ItemStack previousInput = craftingInventory.getItem(slot);
            if (previousInput.isEmpty()) continue;

            int removed = InventoryLib.removeItems(source, input -> {
                // Candidate ingredient must be same item
                if (!ItemStack.isSameItem(input, previousInput)) return false;

                // Recipe must still function with new input swapped in
                craftingInventory.setItem(slot, input);
                var tmpCraftingInput = craftingInventory.asCraftInput();
                boolean canStillCraft =
                        recipe.value().matches(tmpCraftingInput, inputSource.getWorld()) &&
                        ItemStack.isSameItem(result, recipe.value().assemble(tmpCraftingInput, inputSource.getWorld().registryAccess()));
                craftingInventory.setItem(slot, previousInput);

                return canStillCraft;
            }, 1, false);

            if (removed == 0) {
                missingIngredientMask |= 1 << i;
            }
        }

        if (missingIngredientMask != 0) {
            return CraftingResult.missingIngredients(missingIngredientMask);
        }

        // Obtain remaining items using the crafting player hook if player object was provided.
        // (See ResultSlot#onTake(Player, ItemStack))
        //noinspection DataFlowIssue
        CommonHooks.setCraftingPlayer(player);
        NonNullList<ItemStack> remainingStacks = recipe.value().getRemainingItems(posCraftingInput.input()); // Skip re-searching for recipe, should be ok
        //noinspection DataFlowIssue
        CommonHooks.setCraftingPlayer(null);

        return CraftingResult.success(result, remainingStacks);
    }

    private OverlayContainer createAvailableStorageOverlay() {
        var builder = OverlayContainer.builder()
                .addItems(inputSource.getStorage());
        if (inputSource.canConsumeFromCraftingMatrix()) {
            builder.addItems(inputSource.getCraftingMatrix());
        }

        return builder.build();
    }
    //endregion

    /**
     * Holds result of a crafting attempt. Internal.
     */
    private static final class CraftingResult {

        private static final CraftingResult EMPTY = new CraftingResult(ItemStack.EMPTY, NonNullList.create(), 0);

        private final ItemStack outputStack;
        private final NonNullList<ItemStack> remainingItems;
        private final int missingIngredientMask;

        private CraftingResult(ItemStack outputStack, NonNullList<ItemStack> remainingItems, int missingIngredientMask) {
            this.outputStack = outputStack;
            this.remainingItems = remainingItems;
            this.missingIngredientMask = missingIngredientMask;
        }

        public boolean isCraftable() {
            return !outputStack.isEmpty() && missingIngredientMask == 0;
        }

        public NonNullList<ItemStack> getOutputAndRemaining() {
            NonNullList<ItemStack> allResults = NonNullList.withSize(remainingItems.size() + 1, ItemStack.EMPTY);
            int i = 0;
            allResults.set(i++, outputStack.copy());
            for (ItemStack stack : remainingItems) {
                allResults.set(i++, stack.copy());
            }

            return allResults;
        }

        public NonNullList<ItemStack> getRemainingItems() {
            NonNullList<ItemStack> copy = NonNullList.withSize(remainingItems.size(), ItemStack.EMPTY);
            for (int i = 0; i < remainingItems.size(); i++) {
                copy.set(i, remainingItems.get(i).copy());
            }
            return copy;
        }

        public static CraftingResult missingIngredients(int missingIngredientMask) {
            return new CraftingResult(ItemStack.EMPTY, NonNullList.create(), missingIngredientMask);
        }

        public static CraftingResult success(ItemStack outputStack, NonNullList<ItemStack> remainingItems) {
            return new CraftingResult(outputStack, remainingItems, 0);
        }
    }

    /**
     * The holder of a CraftingHelper. Provides access to required objects such as crafting matrix,
     * storage container, and level. Also provides some configurations.
     */
    public interface InventorySource {

        Container getCraftingMatrix();

        Container getStorage();

        default boolean canConsumeFromCraftingMatrix() {
            return false;
        }

        Level getWorld(); // Required for recipe lookup
    }

    /**
     * Maps the slot index from within a CraftingInput (which is a sub-square within a crafting container)
     * into a slot index of this outer crafting container.
     * <p>
     * For example, if the matrix is 3x3, and the crafting input is 2x2 in bottom left, then
     * slot 0 in the 2x2 input corresponds with slot 4 in the 3x3 matrix.
     *
     * @param matrix      The CraftingContainer that the CraftingInput is in
     * @param pCraftInput A CraftingInput positioned somewhere inside matrix
     * @param pSlot       A slot index inside pCraftInput
     * @return The corresponding slot index in matrix
     */
    private static int craftingInputSlotToContainer(CraftingContainer matrix, CraftingInput.Positioned pCraftInput, int pSlot) {
        // xy within the crafting input
        int ix = pSlot % pCraftInput.input().width();
        int iy = pSlot / pCraftInput.input().width();
        // xy within outer grid
        int x = pCraftInput.left() + ix;
        int y = pCraftInput.top() + iy;
        // Outer index
        return y * matrix.getWidth() + x;
    }
}
