package mrtjp.projectred.core.libmc.recipe

import codechicken.microblock.{BlockMicroMaterial, ItemMicroPart, FaceMicroFactory}
import mrtjp.core.item.ItemKeyStack
import net.minecraft.block.Block
import net.minecraft.item.{Item, ItemStack}
import net.minecraftforge.oredict.OreDictionary

class ItemIn(val key:ItemKeyStack) extends Input
{
    def this(s:ItemStack) = this(ItemKeyStack.get(s))
    def this(b:Block) = this(new ItemStack(b))
    def this(i:Item) = this(new ItemStack(i))

    private var nbt = true
    def matchNBT(flag:Boolean):this.type = {nbt = flag; this}

    override def matches(that:ItemKeyStack) = key.key.item == that.key.item &&
        (!nbt || key.key.tag == that.key.tag) &&
        (key.key.itemDamage == OreDictionary.WILDCARD_VALUE || that.key.itemDamage == OreDictionary.WILDCARD_VALUE ||
            key.key.itemDamage == that.key.itemDamage)

    val ins = Seq(key.makeStack)
    override def matchingInputs = ins
}

import net.minecraftforge.oredict.OreDictionary._

import scala.collection.JavaConversions._
class OreIn(val oreIDs:Seq[Int]) extends Input
{
    def this(id:Int) = this(Seq(id))
    def this(name:String) = this(getOreID(name))
    def this(stack:ItemStack) = this(getOreIDs(stack))
    def this(b:Block) = this(new ItemStack(b))
    def this(i:Item) = this(new ItemStack(i))
    def this(stack:ItemKeyStack) = this(stack.makeStack)

    def matches(that:ItemKeyStack) =
    {
        getOreIDs(that.makeStack).exists(oreIDs contains _)
    }

    val ins = oreIDs.map(getOreName).map(getOres).flatten
    override def matchingInputs = ins
}

object MicroIn
{
    //class ids
    def face = FaceMicroFactory.getFactoryID
    def hollowFace = FaceMicroFactory.getFactoryID
    def corner = FaceMicroFactory.getFactoryID
    def edge = FaceMicroFactory.getFactoryID

    //sizes
    val eight = 1
    val fourth = 2
    val half = 4
}

class MicroIn(factoryID:Int, size:Int, material:String) extends Input
{
    def this(c:Int, s:Int, b:Block) = this(c, s, BlockMicroMaterial.materialKey(b))

    private val sample = ItemKeyStack.get(ItemMicroPart.create(factoryID, size, material))
    private val ins = Seq(sample.makeStack)

    override def matches(that:ItemKeyStack) = that.key == sample.key
    override def matchingInputs = ins
}