package mrtjp.projectred.expansion

import mrtjp.core.item.ItemKeyStack
import mrtjp.projectred.core._
import net.minecraft.item.crafting.FurnaceRecipes
import net.minecraft.item.{ItemFood, ItemStack}
import net.minecraftforge.oredict.OreDictionary

object InductiveFurnaceRecipeLib
{
    var recipes = IndexedSeq[InductiveFurnaceRecipe]()

    def getRecipeFor(in:ItemStack):InductiveFurnaceRecipe =
    {
        val key = ItemKeyStack.get(in)
        for (r <- recipes) if (r.in.matches(key)) return r
        null
    }

    def getRecipeOf(out:ItemStack):InductiveFurnaceRecipe =
    {
        val key = ItemKeyStack.get(out)
        for (r <- recipes) if (r.out.matches(key)) return r
        null
    }

    def addRecipe(in:ItemStack, out:ItemStack, ticks:Int)
    {
        recipes :+= InductiveFurnaceRecipe(new ItemIn(in), new ItemOut(out), ticks)
    }

    def addOreRecipe(in:ItemStack, out:ItemStack, ticks:Int)
    {
        recipes :+= InductiveFurnaceRecipe(new OreIn(in), new ItemOut(out), ticks)
    }

    def addOreRecipe(in:String, out:ItemStack, ticks:Int)
    {
        recipes :+= InductiveFurnaceRecipe(new OreIn(in), new ItemOut(out), ticks)
    }

    def init()
    {
        import scala.collection.JavaConversions._

        def isDust(stack:ItemStack) = getOreName(stack).startsWith("dust")
        def isIngot(stack:ItemStack) = getOreName(stack).startsWith("ingot")
        def getOreName(stack:ItemStack) = {
            val IDs = OreDictionary.getOreIDs(stack)
            if(IDs.isEmpty) "Unknown" else OreDictionary.getOreName(IDs(0))
        }

        val sl = FurnaceRecipes.instance.getSmeltingList
        for ((in, out) <- sl) try
        {
            if (getRecipeFor(in) == null)
            {
                if (in.getItem.isInstanceOf[ItemFood]) addRecipe(in, out, 40)
                else if (isDust(in) && isIngot(out)) addOreRecipe(in, out, 80*10/16)
                else addRecipe(in, out, 80)
            }
        }
        catch {
            case e:Exception =>
        }
    }
}

case class InductiveFurnaceRecipe(in:RecipeInput, out:RecipeOutput, burnTime:Int)
{
    def createOutput = out.createOutput
}
