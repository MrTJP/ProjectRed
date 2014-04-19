package mrtjp.projectred.core.libmc

import scala.collection.TraversableOnce
import net.minecraftforge.oredict.{ShapedOreRecipe, OreDictionary}
import net.minecraft.item.ItemStack
import net.minecraft.init.Blocks
import net.minecraft.inventory.InventoryCrafting
import net.minecraft.world.World

class RecipeLib
{
    def newShapedBuilder = new ShapedOreRecipeBuilder
}

trait RecipeBuilder[In <: Input, Out <: Output]
{
    protected var inputs = Vector.newBuilder[In]
    protected var outputs = Vector.newBuilder[Out]

    def +=(elem:In):this.type = {inputs += elem;this}
    def +=(elem1:In, elem2:In, elems:In*):this.type = this += elem1 += elem2 ++= elems
    def ++=(xs:TraversableOnce[In]):this.type = {xs.seq foreach += ;this}

    def -->(elem:Out):this.type = {outputs += elem;this}
    def -->(elem1:Out, elem2:Out, elems:Out*):this.type = this --> elem1 --> elem2 +--> elems
    def +-->(xs:TraversableOnce[Out]):this.type = {xs.seq foreach --> ;this}

    def clear()
    {
        inputs.clear()
        outputs.clear()
        map = ""
        inputMap = Map()
        outputMap = Map()
    }

    protected var map = ""
    def <->(m:String):this.type = {map = m;this}

    protected var inResult:Vector[In] = null
    protected var outResult:Vector[Out] = null
    protected var inputMap = Map[Int, In]()
    protected var outputMap = Map[Int, Out]()

    protected def subResult():this.type =
    {
        inResult = inputs.result()
        outResult = outputs.result()

        val inMB = Map.newBuilder[Int, In]
        val outMB = Map.newBuilder[Int, Out]
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
trait BuildResult[Result] extends RecipeBuilder[Input, Output]
{
    def result:Result
}
trait TRecipeCommons
{
    var id = ""
    def <->(i:String):this.type = {id = i.substring(0,1);this}

    def matches(that:ItemKeyStack):Boolean
}
trait Input extends TRecipeCommons
trait Output extends TRecipeCommons
{
    def createOutput:ItemStack
}

class OreIn(val oreID:String) extends Input
{
    def matches(that:ItemKeyStack) =
    {
        val thatID = OreDictionary.getOreName(OreDictionary.getOreID(that.makeStack))
        thatID != "Unknown" && oreID == thatID
    }
}

class ItemIn(val key:ItemKeyStack) extends Input
{
    private var nbt = true
    def matchNBT(flag:Boolean):this.type = {nbt = flag; this}

    override def matches(that:ItemKeyStack) = key.key.item == that.key.item &&
        (!nbt || key.key.tag == that.key.tag) &&
        (key.key.itemDamage == OreDictionary.WILDCARD_VALUE || that.key.itemDamage == OreDictionary.WILDCARD_VALUE ||
            key.key.itemDamage == that.key.itemDamage)
}

class ItemOut(val key:ItemKeyStack) extends Output
{
    override def matches(that:ItemKeyStack) =
        key == that

    override def createOutput = key.makeStack
}

class ShapedOreRecipeBuilder extends RecipeBuilder[Input, Output] with BuildResult[ShapedOreRecipe]
{
    override def result =
    {
        subResult()
        new ShapedOreRecipe(new ItemStack(Blocks.stone), Array())
        {
            override def getRecipeSize = Math.sqrt(map.length).asInstanceOf[Int]

            override def getCraftingResult(var1:InventoryCrafting) = getRecipeOutput
            override def getRecipeOutput = outResult(0).createOutput

            override def getInput =
            {
                val ins = new Array[Object](9)
                for ((k, v) <- inputMap) ins(k) = v match
                {
                    case in:ItemIn => in.key.makeStack
                    case in:OreIn => in.oreID
                    case _ => null
                }
                ins
            }

            override def matches(inv:InventoryCrafting, world:World):Boolean =
            {
                for (i <- 0 until inv.getSizeInventory)
                {
                    val in = inputMap.getOrElse(i, null)
                    val slot = inv.getStackInSlot(i)
                    if (in == null) if (slot != null) return false
                    if (!in.matches(ItemKeyStack(slot))) return false
                }
                true
            }
        }
    }
}