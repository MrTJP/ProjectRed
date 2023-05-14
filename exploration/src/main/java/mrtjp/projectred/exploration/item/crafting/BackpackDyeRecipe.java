package mrtjp.projectred.exploration.item.crafting;

import mrtjp.projectred.exploration.item.BackpackItem;
import net.minecraft.inventory.CraftingInventory;
import net.minecraft.item.DyeColor;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.IRecipeSerializer;
import net.minecraft.item.crafting.SpecialRecipe;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.World;
import net.minecraftforge.common.Tags;

import static mrtjp.projectred.exploration.init.ExplorationReferences.BACKPACK_DYE_RECIPE_SERIALIZER;
import static mrtjp.projectred.exploration.init.ExplorationReferences.getBackpackByColor;

public class BackpackDyeRecipe extends SpecialRecipe {

    public BackpackDyeRecipe(ResourceLocation id) {
        super(id);
    }

    @Override
    public boolean matches(CraftingInventory inventory, World world) {

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
            } else if (stack.getItem().is(Tags.Items.DYES)) {
                if (!dye.isEmpty()) return false;
                dye = stack;
            }
        }

        if (itemCount != 2 || backpack.isEmpty() || dye.isEmpty()) return false; // Must be exactly 1 backpack and 1 dye

        DyeColor backpackColor = ((BackpackItem) backpack.getItem()).getDyeColor();
        DyeColor dyeColor = DyeColor.getColor(dye);

        if (backpackColor == null || dyeColor == null) return false;

        return backpackColor != dyeColor; // Can't dye to same color
    }

    @Override
    public ItemStack assemble(CraftingInventory inventory) {
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
            } else if (stack.getItem().is(Tags.Items.DYES)) {
                if (!dye.isEmpty()) return ItemStack.EMPTY;
                dye = stack;
            }
        }

        if (itemCount != 2 || backpack.isEmpty() || dye.isEmpty()) return ItemStack.EMPTY; // Must be exactly 1 backpack and 1 dye

        DyeColor backpackColor = ((BackpackItem) backpack.getItem()).getDyeColor();
        DyeColor dyeColor = DyeColor.getColor(dye);

        if (backpackColor == null || dyeColor == null || backpackColor == dyeColor) return ItemStack.EMPTY;

        ItemStack result = new ItemStack(getBackpackByColor(dyeColor.getId()));
        if (backpack.hasTag()) {
            result.setTag(backpack.getTag().copy());
        }

        return result;
    }

    @Override
    public boolean canCraftInDimensions(int x, int y) {
        return x * y >= 2;
    }

    @Override
    public IRecipeSerializer<?> getSerializer() {
        return BACKPACK_DYE_RECIPE_SERIALIZER;
    }
}
