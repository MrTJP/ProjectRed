package mrtjp.projectred.exploration.item.crafting;

import mrtjp.projectred.exploration.init.ExplorationItems;
import mrtjp.projectred.exploration.init.ExplorationRecipeSerializers;
import mrtjp.projectred.exploration.item.BackpackItem;
import net.minecraft.core.RegistryAccess;
import net.minecraft.world.inventory.CraftingContainer;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.CraftingBookCategory;
import net.minecraft.world.item.crafting.CustomRecipe;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.level.Level;
import net.neoforged.neoforge.common.Tags;

import java.util.Objects;

public class BackpackDyeRecipe extends CustomRecipe {

    public BackpackDyeRecipe(CraftingBookCategory category) {
        super(category);
    }

    @Override
    public boolean matches(CraftingContainer inventory, Level world) {

        ItemStack backpack = ItemStack.EMPTY;
        ItemStack dye = ItemStack.EMPTY;
        int itemCount = 0;

        for (int i = 0; i < inventory.getContainerSize(); i++) {
            ItemStack stack = inventory.getItem(i);
            if (stack.isEmpty()) continue;

            itemCount++;

            if (stack.getItem() instanceof BackpackItem) {
                if (!backpack.isEmpty()) return false;
                backpack = stack;
            } else if (stack.is(Tags.Items.DYES)) {
                if (!dye.isEmpty()) return false;
                dye = stack;
            }
        }

        if (itemCount != 2 || backpack.isEmpty() || dye.isEmpty()) return false; // Must be exactly 1 backpack and 1 dye

        DyeColor backpackColor = ((BackpackItem) backpack.getItem()).getDyeColor();
        DyeColor dyeColor = DyeColor.getColor(dye);

        if (dyeColor == null) return false;

        return backpackColor != dyeColor; // Can't dye to same color
    }

    @Override
    public ItemStack assemble(CraftingContainer inventory, RegistryAccess registryAccess) {
        ItemStack backpack = ItemStack.EMPTY;
        ItemStack dye = ItemStack.EMPTY;
        int itemCount = 0;

        for (int i = 0; i < inventory.getContainerSize(); i++) {
            ItemStack stack = inventory.getItem(i);
            if (stack.isEmpty()) continue;

            itemCount++;

            if (stack.getItem() instanceof BackpackItem) {
                if (!backpack.isEmpty()) return ItemStack.EMPTY;
                backpack = stack;
            } else if (stack.is(Tags.Items.DYES)) {
                if (!dye.isEmpty()) return ItemStack.EMPTY;
                dye = stack;
            }
        }

        if (itemCount != 2 || backpack.isEmpty() || dye.isEmpty()) return ItemStack.EMPTY; // Must be exactly 1 backpack and 1 dye

        DyeColor backpackColor = ((BackpackItem) backpack.getItem()).getDyeColor();
        DyeColor dyeColor = DyeColor.getColor(dye);

        if (dyeColor == null || backpackColor == dyeColor) return ItemStack.EMPTY;

        ItemStack result = new ItemStack(ExplorationItems.getBackpackByColor(dyeColor.getId()));
        if (backpack.hasTag()) {
            result.setTag(Objects.requireNonNull(backpack.getTag()).copy());
        }

        return result;
    }

    @Override
    public boolean canCraftInDimensions(int x, int y) {
        return x * y >= 2;
    }

    @Override
    public RecipeSerializer<?> getSerializer() {
        return ExplorationRecipeSerializers.BACKPACK_DYE_RECIPE_SERIALIZER.get();
    }
}
