package mrtjp.projectred.core.libmc.recipe

import codechicken.microblock.ItemMicroPart
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

object OreIn
{
    private def getOreName(stack:ItemStack) =
    {
        val IDs = OreDictionary.getOreIDs(stack)
        if (IDs.length == 0) "Unknown" else OreDictionary.getOreName(IDs(0))
    }
}

class OreIn(val oreID:String) extends Input
{
    def this(stack:ItemKeyStack) =
        this(OreIn.getOreName(stack.makeStack))

    def matches(that:ItemKeyStack) =
    {
        val thatID = OreIn.getOreName(that.makeStack)
        thatID != "Unknown" && oreID == thatID
    }

    import scala.collection.JavaConversions._
    val ins:Seq[ItemStack] = OreDictionary.getOres(oreID)
    override def matchingInputs = ins
}

object MicroIn
{
    val face = 0
    val hollowFace = 1
    val corner = 2
    val edge = 3

    val eight = 1
    val fourth = 2
    val half = 4
}

class MicroIn(classID:Int, size:Int, material:String) extends Input
{
    def this(c:Int, s:Int, b:Block) = this(c, s, Block.blockRegistry.getNameForObject(b))

    private val damage = classID<<8|size&0xFF
    private val sample = ItemKeyStack.get(ItemMicroPart.create(damage, material))
    private val ins = Seq(sample.makeStack)

    override def matches(that:ItemKeyStack) = that.key == sample.key
    override def matchingInputs = ins
}