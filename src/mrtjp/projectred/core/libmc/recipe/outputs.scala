package mrtjp.projectred.core.libmc.recipe

import mrtjp.core.item.ItemKeyStack
import net.minecraft.block.Block
import net.minecraft.item.{Item, ItemStack}

class ItemOut(val key:ItemKeyStack) extends Output
{
    def this(s:ItemStack) = this(ItemKeyStack.get(s))
    def this(b:Block) = this(new ItemStack(b))
    def this(i:Item) = this(new ItemStack(i))

    override def matches(that:ItemKeyStack) =
        key == that

    override def createOutput = key.makeStack
}
