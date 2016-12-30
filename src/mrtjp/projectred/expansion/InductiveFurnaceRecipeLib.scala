package mrtjp.projectred.expansion

import java.util.{Map => JMap}

import mrtjp.core.item.ItemKeyStack
import mrtjp.projectred.core.libmc.recipe._
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

    def addRecipe(r:InductiveFurnaceRecipe)
    {
        recipes :+= r
    }

    def addRecipe(in:ItemStack, out:ItemStack, ticks:Int)
    {
        val b = new InductiveFurnaceRecipeBuilder
        b += new ItemIn(in)
        b += new ItemOut(out)
        b.setBurnTime(ticks)
        b.registerResult()
    }

    def addOreRecipe(in:ItemStack, out:ItemStack, ticks:Int)
    {
        val b = new InductiveFurnaceRecipeBuilder
        b += new OreIn(in)
        b += new ItemOut(out)
        b.setBurnTime(ticks)
        b.registerResult()
    }

    def addOreRecipe(in:String, out:ItemStack, ticks:Int)
    {
        val b = new InductiveFurnaceRecipeBuilder
        b += new OreIn(in)
        b += new ItemOut(out)
        b.setBurnTime(ticks)
        b.registerResult()
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

        val sl = FurnaceRecipes.instance.getSmeltingList.asInstanceOf[JMap[ItemStack, ItemStack]]
        for ((in, out) <- sl) try
        {
            if (getRecipeFor(in) == null)
            {
                if (in.getItem.isInstanceOf[ItemFood]) addRecipe(in, out, 40)
                else if (isDust(in) && isIngot(out)) addOreRecipe(in, out, 80*10/16)
                else if (OreDictionary.getOreIDs(in).nonEmpty) addOreRecipe(in, out, 80)
                else addRecipe(in, out, 80)
            }
        }
        catch {
            case e:Exception =>
        }
    }
}

class InductiveFurnaceRecipeBuilder extends RecipeBuilder
{
    private var ticks = 0
    def setBurnTime(t:Int):this.type = {ticks = t; this}

    def result() =
    {
        compute()
        new InductiveFurnaceRecipe(inResult.head, outResult.head, ticks)
    }

    def registerResult()
    {
        InductiveFurnaceRecipeLib.addRecipe(result())
    }
}

case class InductiveFurnaceRecipe(in:Input, out:Output, burnTime:Int)
{
    def createOutput = out.createOutput
}
