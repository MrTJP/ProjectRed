//package mrtjp.projectred.core
//
//import java.util.{ArrayList => JAList, List => JList}
//
//import net.minecraft.block.Block
//import net.minecraft.inventory.InventoryCrafting
//import net.minecraft.item.{Item, ItemStack}
//import net.minecraft.world.World
//import net.minecraftforge.oredict.{OreDictionary, ShapedOreRecipe, ShapelessOreRecipe}
//
//import scala.collection.JavaConversions._
//import scala.util.control.Breaks._
//
//class ShapelessNBTSensitiveRecipe(result:ItemStack, recipe:AnyRef*) extends ShapelessOreRecipe(result, recipe:_*)
//{
//    override def matches(var1:InventoryCrafting, world:World):Boolean =
//    {
//        val required = new JAList(input)
//
//        for (x <- 0 until var1.getSizeInventory) {
//            val slot = var1.getStackInSlot(x)
//            if (!slot.isEmpty) {
//                var inRecipe = false
//                val req = required.iterator
//
//                breakable { while (req.hasNext) {
//                    var doesMatch = false
//                    val next = req.next
//
//                    next match {
//                        case i:ItemStack =>
//                            doesMatch = ShapedNBTSensitiveRecipe.itemMatches(i, slot, false)
//                        case l:JList[_] =>
//                            val itr = l.asInstanceOf[JList[ItemStack]].iterator
//                            while (itr.hasNext && !doesMatch)
//                                doesMatch = ShapedNBTSensitiveRecipe.itemMatches(itr.next, slot, false)
//                        case _ =>
//                    }
//
//                    if (doesMatch) {
//                        inRecipe = true
//                        required.remove(next)
//                        break()
//                    }
//                }}
//
//                if (!inRecipe)
//                    return false
//            }
//        }
//
//        required.isEmpty
//    }
//
//}
//
//class ShapedNBTSensitiveRecipe(result:ItemStack, recipe:AnyRef*) extends ShapedOreRecipe(result, recipe:_*)
//{
//    def this(result:Block, recipe:AnyRef*) = this(new ItemStack(result), recipe:_*)
//    def this(item:Item, recipe:AnyRef*) = this(new ItemStack(item), recipe:_*)
//
//    override def checkMatch(inv:InventoryCrafting, startX:Int, startY:Int, mirror:Boolean):Boolean =
//    {
//        for (x <- 0 until ShapedOreRecipe.MAX_CRAFT_GRID_WIDTH)
//            for (y <- 0 until ShapedOreRecipe.MAX_CRAFT_GRID_HEIGHT) {
//                val subX:Int = x - startX
//                val subY:Int = y - startY
//                var target:Any = null
//
//                if (subX >= 0 && subY >= 0 && subX < width && subY < height)
//                    if (mirror)
//                        target = input(width - subX - 1 + subY * width)
//                    else
//                        target = input(subX + subY * width)
//
//                val slot = inv.getStackInRowAndColumn(x, y)
//
//                target match {
//                    case stack:ItemStack =>
//                        if (!ShapedNBTSensitiveRecipe.itemMatches(stack, slot, false))
//                            return false
//                    case list:JList[_] =>
//                        if (!list.asInstanceOf[JList[ItemStack]].exists{s =>
//                            ShapedNBTSensitiveRecipe.itemMatches(s, slot, false)
//                        })
//                            return false
//                    case null =>
//                        if (!slot.isEmpty)
//                            return false
//                }
//            }
//        true
//    }
//}
//
//object ShapedNBTSensitiveRecipe
//{
//    def itemMatches(target:ItemStack, input:ItemStack, strict:Boolean):Boolean =
//    {
//        if (!OreDictionary.itemMatches(target, input, strict))
//            return false
//
//        if (target.getTagCompound == null || input.getTagCompound == null)
//            return target.getTagCompound == input.getTagCompound
//
//        target.getTagCompound equals input.getTagCompound
//    }
//}
