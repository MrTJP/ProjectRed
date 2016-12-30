package mrtjp.projectred.core.libmc.recipe

import java.awt.Rectangle

import codechicken.nei.api.stack.PositionedStack
import codechicken.nei.recipe.TemplateRecipeHandler
import codechicken.nei.util.{NEIClientUtils, NEIServerUtils}
import mrtjp.core.item.ItemKeyStack
import net.minecraft.client.gui.inventory.GuiCrafting
import net.minecraft.inventory.InventoryCrafting
import net.minecraft.item.ItemStack
import net.minecraft.item.crafting.{CraftingManager, IRecipe}
import net.minecraft.world.World
import net.minecraftforge.fml.common.registry.GameRegistry

import scala.collection.JavaConversions._

class ShapedRecipeBuilder extends RecipeBuilder with TMappedRecipeBuilder
{
    var size = 3
    def warp(s:Int):this.type = {size = s; this}

    def result() =
    {
        compute()
        new ShapedBuilderRecipe(this)
    }

    def registerResult():this.type =
    {
        GameRegistry.addRecipe(result())
        this
    }
}

class ShapedBuilderRecipe(val builder:ShapedRecipeBuilder) extends IRecipe
{
    override def getRecipeSize = builder.map.length

    override def getCraftingResult(var1:InventoryCrafting) = getRecipeOutput

    override def getRecipeOutput = builder.outResult.head.createOutput

    override def matches(inv:InventoryCrafting, world:World):Boolean =
    {
        if (inv.getSizeInventory < getRecipeSize) return false

        for (i <- 0 until inv.getSizeInventory)
        {
            val in = builder.inputMap.getOrElse(i, null)
            val slot = ItemKeyStack.get(inv.getStackInSlot(i))
            if ((in == null && slot != null) ||
                (slot == null && in != null) ||
                (slot != null && in != null && !in.matches(slot))) return false
        }
        true
    }

    override def getRemainingItems(inv:InventoryCrafting) = null //TODO
}

class PRShapedRecipeHandler extends TemplateRecipeHandler
{
    class CachedShapedRecipe(r:ShapedBuilderRecipe) extends CachedRecipe
    {
        var inputs:Seq[PositionedStack] =
        {
            val ins = Seq.newBuilder[PositionedStack]
            for (x <- 0 until r.builder.size) for (y <- 0 until r.builder.size)
            {
                val idx = y*r.builder.size+x
                r.builder.inputMap.get(idx) match
                {
                    case Some(in) =>
                        val stack = new PositionedStack(in.matchingInputs.toArray, 25+x*18, 6+y*18, false)
                        stack.setMaxSize(1)
                        ins += stack
                    case None =>
                }
            }
            ins.result()
        }

        val outputs = new PositionedStack(r.getRecipeOutput, 119, 24)

        override def getIngredients = getCycledIngredients(cycleticks/20, inputs)

        override def getResult = outputs

        def computeVisuals(){for (s <- inputs) s.generatePermutations()}
    }

    override def loadTransferRects()
    {
        transferRects.add(new TemplateRecipeHandler.RecipeTransferRect(new Rectangle(84, 23, 24, 18), "crafting"))
    }

    override def loadCraftingRecipes(result:ItemStack)
    {
        for (irecipe:IRecipe <- CraftingManager.getInstance.getRecipeList)
        {
            if (NEIServerUtils.areStacksSameTypeCrafting(irecipe.getRecipeOutput, result)) irecipe match
            {
                case r:ShapedBuilderRecipe =>
                    val cr = new CachedShapedRecipe(r)
                    cr.computeVisuals()
                    arecipes.add(cr)
                case _ =>
            }
        }
    }

    override def loadUsageRecipes(ingredient:ItemStack)
    {
        for (irecipe <- CraftingManager.getInstance.getRecipeList) irecipe match
        {
            case r:ShapedBuilderRecipe =>
                val cr = new CachedShapedRecipe(r)
                if (cr.contains(cr.inputs, ingredient.getItem))
                {
                    cr.computeVisuals()
                    if (cr.contains(cr.inputs, ingredient))
                    {
                        cr.setIngredientPermutation(cr.inputs, ingredient)
                        arecipes.add(cr)
                    }
                }
            case _ =>
        }
    }

    override def getGuiTexture = "textures/gui/container/crafting_table.png"
    override def getRecipeName = NEIClientUtils.translate("recipe.shaped")
    override def getOverlayIdentifier = "crafting"
    override def getGuiClass = classOf[GuiCrafting]
}
