package mrtjp.projectred.core.libmc.recipe

import java.util.{List => JList}

import codechicken.nei.api.stack.PositionedStack
import codechicken.nei.util.{NEIClientUtils, NEIServerUtils}
import mrtjp.core.item.ItemKeyStack
import net.minecraft.inventory.InventoryCrafting
import net.minecraft.item.ItemStack
import net.minecraft.item.crafting.{CraftingManager, IRecipe}
import net.minecraft.world.World
import net.minecraftforge.fml.common.registry.GameRegistry

import scala.collection.JavaConversions._

class ShapelessRecipeBuilder extends RecipeBuilder// with TMappedRecipeBuilder
{
    def result() =
    {
        compute()
        new ShapelessBuilderRecipe(this)
    }

    def registerResult():this.type =
    {
        GameRegistry.addRecipe(result())
        this
    }
}

class ShapelessBuilderRecipe(val builder:ShapelessRecipeBuilder) extends IRecipe
{
    override def getRecipeSize = builder.inResult.size

    override def getRecipeOutput = builder.outResult.head.createOutput

    override def getCraftingResult(inv:InventoryCrafting) = getRecipeOutput

    override def matches(inv:InventoryCrafting, var2:World):Boolean =
    {
        if (inv.getSizeInventory < getRecipeSize) return false

        var required = builder.inResult
        for (i <- 0 until inv.getSizeInventory)
        {
            val stack = ItemKeyStack.get(inv.getStackInSlot(i))
            if (stack != null) required.find(_.matches(stack)) match
            {
                case Some(e) =>
                    val idx = required.indexOf(e)
                    required = required.take(idx) ++ required.drop(idx+1)
                case None => return false
            }
        }
        required.isEmpty
    }

    override def getRemainingItems(inv:InventoryCrafting) = null //TODO
}

class PRShapelessRecipeHandler extends PRShapedRecipeHandler
{
    class CachedShapelessRecipe(r:ShapelessBuilderRecipe) extends CachedRecipe
    {
        var stackorder = Array[Array[Int]](
            Array(0, 0), Array(1, 0), Array(0, 1),
            Array(1, 1), Array(0, 2), Array(1, 2),
            Array(2, 0), Array(2, 1), Array(2, 2)
        )

        val inputs =
        {
            val b = Seq.newBuilder[PositionedStack]
            for (i <- 0 until r.builder.inResult.size)
            {
                val in = r.builder.inResult(i)
                val stack = new PositionedStack(in.matchingInputs.toArray, 25+stackorder(i)(0)*18, 6+stackorder(i)(1)*18)
                stack.setMaxSize(1)
                b += stack
            }
            b.result()
        }

        val output = new PositionedStack(r.getRecipeOutput, 119, 24)

        override def getResult = output

        override def getIngredients = getCycledIngredients(cycleticks/20, inputs)
    }

    override def getRecipeName = NEIClientUtils.translate("recipe.shapeless")

    override def loadCraftingRecipes(result:ItemStack)
    {
        for (irecipe:IRecipe <- CraftingManager.getInstance.getRecipeList) irecipe match
        {
            case r:ShapelessBuilderRecipe
                if NEIServerUtils.areStacksSameTypeCrafting(irecipe.getRecipeOutput, result) =>
                val recipe = new CachedShapelessRecipe(r)
                arecipes.add(recipe)
            case _ =>
        }
    }

    override def loadUsageRecipes(ingredient:ItemStack)
    {
        for (irecipe <- CraftingManager.getInstance.getRecipeList.asInstanceOf[JList[IRecipe]]) irecipe match
        {
            case r:ShapelessBuilderRecipe =>
                val recipe = new CachedShapelessRecipe(r)
                if (recipe.contains(recipe.inputs, ingredient))
                {
                    recipe.setIngredientPermutation(recipe.inputs, ingredient)
                    arecipes.add(recipe)
                }
            case _ =>
        }
    }
}
