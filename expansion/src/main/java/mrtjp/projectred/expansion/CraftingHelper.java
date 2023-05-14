package mrtjp.projectred.expansion;

import mrtjp.projectred.lib.InventoryLib;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.inventory.CraftResultInventory;
import net.minecraft.inventory.CraftingInventory;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.Inventory;
import net.minecraft.inventory.container.Container;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.ICraftingRecipe;
import net.minecraft.item.crafting.IRecipeType;
import net.minecraft.util.LazyValue;
import net.minecraft.util.NonNullList;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeHooks;

import java.util.function.Predicate;

public class CraftingHelper {

    private final CraftingInventory craftingInventory = new CraftingInventory(new Container(null, -1) {
        @Override
        public boolean stillValid(PlayerEntity p_75145_1_) {
            return false;
        }
    }, 3, 3);

    private final CraftResultInventory craftResultInventory = new CraftResultInventory();

    private final InventorySource inputSource;

    private ICraftingRecipe recipe = null;
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
    public CraftingInventory getCraftingInventory() {
        return craftingInventory;
    }

    public CraftResultInventory getCraftResultInventory() {
        return craftResultInventory;
    }
    //region

    public void loadInputs() {
        IInventory craftingMatrix = inputSource.getCraftingMatrix();
        // Copy recipe matrix to internal Crafting Inventory
        for (int i = 0; i < 9; i++) {
            craftingInventory.setItem(i, craftingMatrix.getItem(i).copy());
        }
    }

    public void loadRecipe() {
        recipe = inputSource.getWorld().getRecipeManager()
                .getRecipeFor(IRecipeType.CRAFTING, craftingInventory, inputSource.getWorld()).orElse(null);

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

    public boolean onCraftedByPlayer(PlayerEntity player, boolean leaveRemainingInGrid) {
        CraftingResult result = craftFromStorage(false);

        if (!result.isCraftable()) {
            return false;
        }

        // Re-obtain remaining items in case "setCraftingPlayer" changes remaining items
        ForgeHooks.setCraftingPlayer(player);
//        NonNullList<ItemStack> remainingStacks = player.level.getRecipeManager().getRemainingItemsFor(IRecipeType.CRAFTING, craftingInventory, player.level);
        NonNullList<ItemStack> remainingStacks = recipe.getRemainingItems(craftingInventory); // Skip re-searching for recipe, should be ok
        ForgeHooks.setCraftingPlayer(null);

        IInventory craftingGird = inputSource.getCraftingMatrix();
        IInventory storage = inputSource.getStorage();

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

        IInventory storage = inputSource.getStorage();
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

    private boolean consumeIngredient(IInventory storage, int startIndex, Predicate<ItemStack> matchFunc) {

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

    private static IInventory copyInventory(IInventory inventory) {
        //TODO create more accurate copy
        Inventory copy = new Inventory(inventory.getContainerSize());
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
        public final IInventory remainingStorage;

        private final LazyValue<Boolean> canStorageAcceptResults = new LazyValue<>(this::canFitResultsIntoStorage);

        public CraftingResult(ItemStack outputStack, NonNullList<ItemStack> remainingItems, int missingIngredientMask, IInventory remainingStorage) {
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
            IInventory storage = copyInventory(remainingStorage); // Don't mutate original list
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

        IInventory getCraftingMatrix();

        IInventory getStorage();

        World getWorld(); // Required for recipe lookup
    }
}
