package mrtjp.projectred.expansion;

import mrtjp.projectred.lib.InventoryLib;
import net.covers1624.quack.util.LazyValue;
import net.minecraft.core.NonNullList;
import net.minecraft.world.Container;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.CraftingContainer;
import net.minecraft.world.inventory.ResultContainer;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.CraftingRecipe;
import net.minecraft.world.item.crafting.RecipeType;
import net.minecraft.world.level.Level;
import net.minecraftforge.common.ForgeHooks;

import java.util.function.Predicate;

public class CraftingHelper {

    private final CraftingContainer craftingInventory = new CraftingContainer(new AbstractContainerMenu(null, -1) {
        @Override
        public boolean stillValid(Player p_75145_1_) {
            return false;
        }
    }, 3, 3);

    private final ResultContainer craftResultInventory = new ResultContainer();

    private final InventorySource inputSource;

    private CraftingRecipe recipe = null;
    private CraftingResult result = CraftingResult.EMPTY;

    public CraftingHelper(InventorySource inputSource) {
        this.inputSource = inputSource;
    }

    //region Inventory events
    public void clear() {
        recipe = null;
        craftingInventory.clearContent();
    }

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

    public void loadInputs() {
        Container craftingMatrix = inputSource.getCraftingMatrix();
        // Copy recipe matrix to internal Crafting Inventory
        for (int i = 0; i < 9; i++) {
            craftingInventory.setItem(i, craftingMatrix.getItem(i).copy());
        }
    }

    public void loadRecipe() {
        recipe = inputSource.getWorld().getRecipeManager()
                .getRecipeFor(RecipeType.CRAFTING, craftingInventory, inputSource.getWorld()).orElse(null);

        craftResultInventory.setItem(0, recipe == null ? ItemStack.EMPTY : recipe.assemble(craftingInventory));
    }

    public void loadOutput() {

        result = craftFromStorage(true);
    }

    public boolean hasRecipe() {
        return recipe != null;
    }

    public ItemStack getRecipeOutout() {
        return craftResultInventory.getItem(0);
    }

    public boolean canTake() {
        return result.isCraftable();
    }

    public boolean canTakeIntoStorage() {
        return canTake() && result.canStorageAcceptResults();
    }

    public int getMissingIngredientMask() {
        return result.missingIngredientMask;
    }

    public boolean onCraftedByPlayer(Player player, boolean leaveRemainingInGrid) {
        CraftingResult result = craftFromStorage(false);

        if (!result.isCraftable()) {
            return false;
        }

        // Re-obtain remaining items in case "setCraftingPlayer" changes remaining items
        ForgeHooks.setCraftingPlayer(player);
//        NonNullList<ItemStack> remainingStacks = player.level.getRecipeManager().getRemainingItemsFor(IRecipeType.CRAFTING, craftingInventory, player.level);
        NonNullList<ItemStack> remainingStacks = recipe.getRemainingItems(craftingInventory); // Skip re-searching for recipe, should be ok
        ForgeHooks.setCraftingPlayer(null);

        Container craftingGird = inputSource.getCraftingMatrix();
        Container storage = inputSource.getStorage();

        for (int i = 0; i < 9; i++) {
            ItemStack remaining = remainingStacks.get(i);
            if (remaining.isEmpty()) continue;

            // If allowed, leave remaining in crafting grid just like Vanilla crafting bench
            if (leaveRemainingInGrid && craftingGird.getItem(i).isEmpty()) {
                craftingGird.setItem(i, remaining.split(remaining.getCount()));
                continue;
            }

            // Otherwise try to put it somewhere
            InventoryLib.injectItemStack(storage, remaining, true); // In storage
            if (!remaining.isEmpty()) player.addItem(remaining);    // In player inventory
            if (!remaining.isEmpty()) player.drop(remaining, false);// Or as last resort, on ground
        }

        return true;
    }

    public boolean onCraftedIntoStorage() {

        CraftingResult result = craftFromStorage(false);

        if (!result.isCraftable() || !result.canFitResultsIntoStorage()) return false;

        NonNullList<ItemStack> allResults = result.getCopyOfAllResults();
        InventoryLib.injectAllItemStacks(inputSource.getStorage(), allResults, true);

        return true;
    }

    private CraftingResult craftFromStorage(boolean simulate) {

        if (recipe == null) return CraftingResult.EMPTY;

        if (!recipe.matches(craftingInventory, inputSource.getWorld())) return CraftingResult.EMPTY;

        ItemStack result = recipe.assemble(craftingInventory);
        if (result.isEmpty()) return CraftingResult.EMPTY;

        Container storage = inputSource.getStorage();
        if (simulate) {
            storage = copyInventory(storage);
        }

        // Try to consume all ingredients
        int missingIngredientMask = 0;
        for (int i = 0; i < 9; i++) {
            final int slot = i;
            ItemStack previousInput = craftingInventory.getItem(slot);
            if (previousInput.isEmpty()) continue;

            boolean isPresent = consumeIngredient(storage, 0, input -> {
                // Candidate ingredient must be same item
                if (!input.sameItemStackIgnoreDurability(previousInput)) return false;

                // Recipe must still function with new input swapped in
                craftingInventory.setItem(slot, input);
                boolean canStillCraft =
                        recipe.matches(craftingInventory, inputSource.getWorld()) &&
                        ItemStack.isSame(result, recipe.assemble(craftingInventory));
                craftingInventory.setItem(slot, previousInput);

                return canStillCraft;
            });

            if (!isPresent) {
                missingIngredientMask |= 1 << i;
            }
        }

        if (missingIngredientMask != 0) {
            return CraftingResult.missingIngredients(missingIngredientMask);
        }

        return new CraftingResult(result, recipe.getRemainingItems(craftingInventory), 0, simulate ? storage : copyInventory(storage));
    }

//    private boolean insertResultsIntoInventory(NonNullList inventory, boolean simulate) {
//        if (result.outputStack.isEmpty() || result.missingIngredientMask != 0) {
//            return false;
//        }
//
//        IInventory storage = copyInventory(result.remainingStorage);
//
//        // Try to insert into remaining storage
//        ItemStack output = outputStack.copy();
//        InventoryLib.injectItemStack(storage, output, true);
//        if (!output.isEmpty()) return false;
//
//        // Try to insert remaining items
//        for (ItemStack stack : remainingItems) {
//            ItemStack remaining = stack.copy();
//            InventoryLib.injectItemStack(storage, remaining, true);
//            if (!remaining.isEmpty()) return false;
//        }
//
//        return true;
//    }

    private boolean consumeIngredient(Container storage, int startIndex, Predicate<ItemStack> matchFunc) {

        int i = startIndex;
        do {
            ItemStack stack = storage.getItem(i);
            if (!stack.isEmpty() && matchFunc.test(stack)) {
                ItemStack taken = storage.removeItem(i, 1);
                if (!taken.isEmpty()) {
                    return true;
                }
            }
            i = (i + 1) % storage.getContainerSize();
        } while (i != startIndex);

        return false;
    }

    private static Container copyInventory(Container inventory) {
        //TODO create more accurate copy
        SimpleContainer copy = new SimpleContainer(inventory.getContainerSize());
        for (int i = 0; i < inventory.getContainerSize(); i++) {
            copy.setItem(i, inventory.getItem(i).copy());
        }
        return copy;
    }

    private static class CraftingResult {

        private static final CraftingResult EMPTY = new CraftingResult(ItemStack.EMPTY, NonNullList.create(), 0, null);

        public final ItemStack outputStack;
        public final NonNullList<ItemStack> remainingItems;
        public final int missingIngredientMask;
        public final Container remainingStorage;

        private final LazyValue<Boolean> canStorageAcceptResults = new LazyValue<>(this::canFitResultsIntoStorage);

        public CraftingResult(ItemStack outputStack, NonNullList<ItemStack> remainingItems, int missingIngredientMask, Container remainingStorage) {
            this.outputStack = outputStack;
            this.remainingItems = remainingItems;
            this.missingIngredientMask = missingIngredientMask;
            this.remainingStorage = remainingStorage;
        }

        public boolean isCraftable() {
            return !outputStack.isEmpty() && missingIngredientMask == 0;
        }

        public boolean canStorageAcceptResults() {
            return canStorageAcceptResults.get();
        }

        public NonNullList<ItemStack> getCopyOfAllResults() {

            NonNullList<ItemStack> allResults = NonNullList.withSize(remainingItems.size() + 1, ItemStack.EMPTY);
            int i = 0;
            allResults.set(i++, outputStack.copy());
            for (ItemStack stack : remainingItems) {
                allResults.set(i++, stack.copy());
            }

            return allResults;
        }

        private boolean canFitResultsIntoStorage() {
            Container storage = copyInventory(remainingStorage); // Don't mutate original list
            return InventoryLib.injectAllItemStacks(storage, getCopyOfAllResults(), true);
        }

        public static CraftingResult empty() {
            return EMPTY;
        }

        public static CraftingResult missingIngredients(int missingIngredientMask) {
            return new CraftingResult(ItemStack.EMPTY, NonNullList.create(), missingIngredientMask, null);
        }
    }

    public interface InventorySource {

        Container getCraftingMatrix();

        Container getStorage();

        Level getWorld(); // Required for recipe lookup
    }
}
