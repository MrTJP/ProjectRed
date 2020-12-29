/*
package mrtjp.projectred.core;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import net.minecraft.inventory.InventoryCrafting;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.IRecipe;
import net.minecraft.item.crafting.Ingredient;
import net.minecraft.util.JsonUtils;
import net.minecraft.util.NonNullList;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.common.crafting.CraftingHelper;
import net.minecraftforge.common.crafting.IRecipeFactory;
import net.minecraftforge.common.crafting.JsonContext;
import net.minecraftforge.oredict.ShapelessOreRecipe;

import javax.annotation.Nonnull;

public class ShapelessOreNBTCopyRecipe extends ShapelessOreRecipe {

    private ItemStack output;

    public ShapelessOreNBTCopyRecipe(ResourceLocation group, NonNullList<Ingredient> input, @Nonnull ItemStack result) {
        super(group, input, result);
        output = result.copy();
    }

    @Override
    public ItemStack getCraftingResult(InventoryCrafting inv) {
        ItemStack out = output.copy();

        ItemStack oldItemWithNBT = ItemStack.EMPTY;
        for (int i = 0; i < 9; i++) {
            ItemStack slot = inv.getStackInSlot(i);
            if (!slot.isEmpty()) {
                if (slot.hasTagCompound()) {
                    oldItemWithNBT = slot;
                    break;
                }
            }
        }
        if (!oldItemWithNBT.isEmpty()) {
            out.setTagCompound(oldItemWithNBT.getTagCompound());
        }

        return out;
    }

    public static class Factory implements IRecipeFactory {

        @Override
        public IRecipe parse(JsonContext context, JsonObject json) {
            String group = JsonUtils.getString(json, "group", "");

            NonNullList<Ingredient> ings = NonNullList.create();
            for (JsonElement ele : JsonUtils.getJsonArray(json, "ingredients"))
                ings.add(CraftingHelper.getIngredient(ele, context));

            if (ings.isEmpty())
                throw new JsonParseException("No ingredients for shapeless recipe");

            ItemStack itemstack = CraftingHelper.getItemStack(JsonUtils.getJsonObject(json, "result"), context);
            return new ShapelessOreNBTCopyRecipe(group.isEmpty() ? null : new ResourceLocation(group), ings, itemstack);
        }
    }
}
*/
