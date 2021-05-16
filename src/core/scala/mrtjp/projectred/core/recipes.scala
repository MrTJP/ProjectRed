package mrtjp.projectred.core

import codechicken.lib.datagen.recipe.ShapelessRecipeBuilder
import com.google.gson.JsonObject
import net.minecraft.inventory.CraftingInventory
import net.minecraft.item.ItemStack
import net.minecraft.item.crafting.{Ingredient, ShapelessRecipe}
import net.minecraft.network.PacketBuffer
import net.minecraft.util.{IItemProvider, NonNullList, ResourceLocation}

import scala.util.control.Breaks.{break, breakable}

//TODO, this needs to validate what slot its grabbing the NBT tag from.
class ShapelessNBTCopyRecipe(id: ResourceLocation, group: String, output: ItemStack, ingredients: NonNullList[Ingredient]) extends ShapelessRecipe(id, group, output, ingredients) {

    def this(other: ShapelessRecipe) = this(other.getId, other.getGroup, other.getResultItem, other.getIngredients)

    override def assemble(inv: CraftingInventory) = {
        var oldItem = ItemStack.EMPTY
        breakable {
            for (i <- 0 until 9) {
                val stack = inv.getItem(i)
                if (!stack.isEmpty && stack.hasTag) {
                    oldItem = stack;
                    break()
                }
            }
        }
        val result = super.assemble(inv);
        if (!oldItem.isEmpty) {
            result.setTag(oldItem.getTag)
        }
        result
    }

    override def getSerializer = CoreContent.shapelessNBTCopyRecipeSerializer.get()
}

class ShapelessNBTCopyRecipeSerializer extends ShapelessRecipe.Serializer {
    override def fromNetwork(recipeId: ResourceLocation, buffer: PacketBuffer) = new ShapelessNBTCopyRecipe(super.fromNetwork(recipeId, buffer))

    override def fromJson(recipeId: ResourceLocation, json: JsonObject) = new ShapelessNBTCopyRecipe(super.fromJson(recipeId, json))
}

class ShapelessNBTCopyRecipeBuilder(id: ResourceLocation, result: ItemStack) extends ShapelessRecipeBuilder(CoreContent.shapelessNBTCopyRecipeSerializer.get(), id, result)

object ShapelessNBTCopyRecipeBuilder {
    def apply(result: IItemProvider): ShapelessNBTCopyRecipeBuilder = apply(result, 1)

    def apply(result: IItemProvider, count: Int): ShapelessNBTCopyRecipeBuilder = apply(new ItemStack(result, count))

    def apply(result: IItemProvider, count: Int, id: ResourceLocation): ShapelessNBTCopyRecipeBuilder = apply(new ItemStack(result, count), id)

    def apply(result: ItemStack): ShapelessNBTCopyRecipeBuilder = apply(result, result.getItem.getRegistryName)

    def apply(result: ItemStack, id: ResourceLocation): ShapelessNBTCopyRecipeBuilder = new ShapelessNBTCopyRecipeBuilder(id, result)
}
