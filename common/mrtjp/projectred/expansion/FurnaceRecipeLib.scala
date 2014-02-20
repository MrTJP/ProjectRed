package mrtjp.projectred.expansion

import mrtjp.projectred.core.utils.{ItemKey, LabelBreaks, ItemKeyStack}
import net.minecraft.block.Block
import net.minecraft.item.crafting.FurnaceRecipes
import net.minecraft.item.{Item, ItemStack}
import net.minecraftforge.oredict.OreDictionary

object FurnaceRecipeLib
{
    var recipes = IndexedSeq[RecipeFurnace]()

    /**
     * Returns the recipe for the specified input if any.
     * @param in Recipe input
     * @return
     */
    def getRecipeFor(in:ItemStack):RecipeFurnace =
    {
        for (r <- recipes) if (r.matchesIn(ItemKey.get(in))) return r
        null
    }

    /**
     * Returns the recipe for the specified output if any
     * @param out Recipe output
     * @return
     */
    def getRecipeOf(out:ItemStack):RecipeFurnace =
    {
        for (r <- recipes) if (r.matchesOut(ItemKey.get(out))) return r
        null
    }

    def addRecipe(in:ItemStack, out:ItemStack, ticks:Int)
    {
        val r = new RecipeFurnace(ItemKey.get(in), ItemKeyStack.get(out), ticks)
        recipes :+= r
    }

    def addOreRecipe(in:ItemStack, out:ItemStack, ticks:Int)
    {
        if (OreDictionary.getOreName(OreDictionary.getOreID(in)) == "Unknown")
        {
            addRecipe(in, out, ticks)
            return
        }
        val r = new RecipeFurnace(ItemKey.get(in), ItemKeyStack.get(out), ticks) with FurnaceOreDicRecipe
        recipes :+= r
    }

    def addOreRecipe(in:String, out:ItemStack, ticks:Int)
    {
        val list = OreDictionary.getOres(in)
        if (!list.isEmpty) addOreRecipe(list.get(0), out, ticks)
    }

    def addOreRecipe(in:Int, out:ItemStack, ticks:Int)
    {
        val list = OreDictionary.getOres(in)
        if (!list.isEmpty) addOreRecipe(list.get(0), out, ticks)
    }

    def recipeExists(input:ItemStack) = getRecipeOf(input) != null

    def init()
    {
        //default
        addRecipe(new ItemStack(Block.cactus), new ItemStack(Item.dyePowder, 1, 2), 800)
        addRecipe(new ItemStack(Item.porkRaw), new ItemStack(Item.porkCooked), 400)
        addRecipe(new ItemStack(Item.fishRaw), new ItemStack(Item.fishCooked), 400)
        addRecipe(new ItemStack(Item.beefRaw), new ItemStack(Item.beefCooked), 400)
        addRecipe(new ItemStack(Item.chickenRaw), new ItemStack(Item.chickenCooked), 400)
        addRecipe(new ItemStack(Item.potato), new ItemStack(Item.bakedPotato), 400)

        import LabelBreaks._
        import scala.collection.JavaConversions._
        val sl2 = FurnaceRecipes.smelting.getMetaSmeltingList
        val sl = FurnaceRecipes.smelting.getSmeltingList

        def isDust(stack:ItemStack) = getOreName(stack).startsWith("dust")
        def isIngot(stack:ItemStack) = getOreName(stack).startsWith("ingot")
        def getOreName(stack:ItemStack) = OreDictionary.getOreName(OreDictionary.getOreID(stack))

        for (key <- sl2.keySet())
        {
            val input = new ItemStack(key.get(0).intValue, 1, key.get(1).intValue)
            val output = sl2.get(key)

            if (isDust(input) && isIngot(output)) addRecipe(input, output, 160*10/16)
            else addRecipe(input, output, 160)
        }

        for (k <- sl.keySet()) label("cont")
        {
            val key = k.asInstanceOf[Integer]

            val input = new ItemStack(key.intValue, 1, 0)
            if (recipeExists(input)) break("cont")

            val output = sl.get(key).asInstanceOf[ItemStack]

            if (isDust(input) && isIngot(output)) addOreRecipe(input, output, 160*10/16)
            else addOreRecipe(input, output, 160)
        }
    }
}

class RecipeFurnace(in:ItemKey, out:ItemKeyStack, val ticks:Int)
{
    def input = in.makeStack(1)
    def output = out.makeStack()

    def inputKey = in.copy()
    def outputKey = out.copy()

    def matchesIn(inputstack:ItemKey) = inputstack == in
    def matchesOut(outputstack:ItemKey) = outputstack == out
}

trait FurnaceOreDicRecipe extends RecipeFurnace
{
    val oreDicID = OreDictionary.getOreID(input)

    override def matchesIn(inputstack:ItemKey) =
        super.matchesIn(inputstack) ||
            (oreDicID > -1 && oreDicID == OreDictionary.getOreID(inputstack.makeStack(1)))
}