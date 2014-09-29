package mrtjp.projectred.expansion

import net.minecraft.item.crafting.FurnaceRecipes
import net.minecraft.item.ItemStack
import net.minecraftforge.oredict.OreDictionary
import mrtjp.projectred.core.libmc.{ItemKeyStack, ItemKey}
import net.minecraft.init.{Items, Blocks}
import java.util.{Map => JMap}

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
        if (OreDictionary.getOreIDs(in).length == 0)
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
        val name = OreDictionary.getOreName(in)
        if ("Unknown" != name) addOreRecipe(OreDictionary.getOres(name).get(0), out, ticks)
    }

    def recipeExists(input:ItemStack) = getRecipeOf(input) != null

    def init()
    {
        //default
        addRecipe(new ItemStack(Blocks.cactus), new ItemStack(Items.dye, 1, 2), 800)
        addRecipe(new ItemStack(Items.porkchop), new ItemStack(Items.cooked_porkchop), 400)
        addRecipe(new ItemStack(Items.fish), new ItemStack(Items.cooked_fished), 400)
        addRecipe(new ItemStack(Items.beef), new ItemStack(Items.cooked_beef), 400)
        addRecipe(new ItemStack(Items.chicken), new ItemStack(Items.cooked_chicken), 400)
        addRecipe(new ItemStack(Items.potato), new ItemStack(Items.baked_potato), 400)

        import scala.collection.JavaConversions._
        val sl = FurnaceRecipes.smelting.getSmeltingList.asInstanceOf[JMap[ItemStack, ItemStack]]

        def isDust(stack:ItemStack) = getOreName(stack).startsWith("dust")
        def isIngot(stack:ItemStack) = getOreName(stack).startsWith("ingot")
        def getOreName(stack:ItemStack) = {
          val IDs = OreDictionary.getOreIDs(stack)
          if (IDs.length == 0) "Unknown" else OreDictionary.getOreName(IDs(0));
        }

        for ((in, out) <- sl)
        {
            if (!recipeExists(in))
            {
                if (isDust(in) && isIngot(out)) addOreRecipe(in, out, 160*10/16)
                else addOreRecipe(in, out, 160)
            }
        }
    }
}

class RecipeFurnace(in:ItemKey, out:ItemKeyStack, val ticks:Int)
{
    def input = in.makeStack(1)
    def output = out.makeStack

    def inputKey = in.copy
    def outputKey = out.copy

    def matchesIn(inputstack:ItemKey) = inputstack == in
    def matchesOut(outputstack:ItemKey) = outputstack == out
}

trait FurnaceOreDicRecipe extends RecipeFurnace
{
    val oreDicID = OreDictionary.getOreIDs(input)

    override def matchesIn(inputstack:ItemKey) =
        super.matchesIn(inputstack) ||
            (oreDicID.length > 0 && oreDicID == OreDictionary.getOreIDs(inputstack.makeStack(1)))
}