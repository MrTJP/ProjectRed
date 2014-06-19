package mrtjp.projectred.core.libmc.recipe

import codechicken.nei.api.{API => neiapi}
import codechicken.nei.recipe.{ICraftingHandler, IUsageHandler}
import mrtjp.projectred.core.libmc.ItemKeyStack
import net.minecraft.item.ItemStack
import net.minecraft.item.crafting.IRecipe
import net.minecraftforge.oredict.RecipeSorter
import net.minecraftforge.oredict.RecipeSorter.Category._

object RecipeLib
{
    def newShapedBuilder = new ShapedRecipeBuilder
    def newShapelessBuilder = new ShapelessRecipeBuilder

    def loadLib()
    {
        RecipeSorter.register("projectred:shaped", classOf[ShapedBuilderRecipe], SHAPED, "after:forge:shaped")
        RecipeSorter.register("projectred:shapeless", classOf[ShapelessBuilderRecipe], SHAPELESS, "after:forge:shapeless")
    }

    def loadNEI()
    {
        def reg(handler:IUsageHandler with ICraftingHandler)
        {
            neiapi.registerRecipeHandler(handler)
            neiapi.registerUsageHandler(handler)
        }

        reg(new PRShapedRecipeHandler)
        reg(new PRShapelessRecipeHandler)
    }
}

trait RecipeBuilder
{
    protected var inputs = Vector.newBuilder[Input]
    protected var outputs = Vector.newBuilder[Output]

    def +=(elem:Input):this.type = {inputs += elem; this}
    def +=(elem:Output):this.type = {outputs += elem; this}

    var map = ""
    def <->(m:String):this.type = {map = m; this}

    var size = 3
    def warp(s:Int):this.type = {size = s; this}

    var inResult:Vector[Input] = _
    var outResult:Vector[Output] = _
    var inputMap:Map[Int, Input] = _
    var outputMap:Map[Int, Output] = _

    protected def compute():this.type =
    {
        inResult = inputs.result()
        outResult = outputs.result()

        val inMB = Map.newBuilder[Int, Input]
        val outMB = Map.newBuilder[Int, Output]
        val sSeq = map.map(c => String.valueOf(c))

        for (i <- 0 until sSeq.length; id = sSeq(i)) if (!id.isEmpty)
        {
            val in = inResult.find(_.id == id)
            val out = outResult.find(_.id == id)
            if (in.isDefined) inMB += i -> in.get
            if (out.isDefined) outMB += i -> out.get
        }

        inputMap = inMB.result()
        outputMap = outMB.result()
        this
    }

}

trait BuildResult[Result <: BuilderRecipe]
{
    def result:Result
    def registerResult():this.type
}

trait TRecipeObject
{
    var id = ""
    def to(i:String):this.type = {id = i.substring(0,1); this}

    def matches(that:ItemKeyStack):Boolean
}

trait Input extends TRecipeObject
{
    def matchingInputs:Seq[ItemStack]
}

trait Output extends TRecipeObject
{
    def createOutput:ItemStack
}

abstract class BuilderRecipe(val builder:RecipeBuilder) extends IRecipe