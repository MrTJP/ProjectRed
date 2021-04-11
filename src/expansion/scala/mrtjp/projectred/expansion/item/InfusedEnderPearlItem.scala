package mrtjp.projectred.expansion.item

import mrtjp.projectred.expansion.ExpansionContent
import net.minecraft.client.util.ITooltipFlag
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.util.math.BlockPos
import net.minecraft.util.text.{ITextComponent, StringTextComponent, TextFormatting}
import net.minecraft.world.World

import java.util

class InfusedEnderPearlItem extends Item(new Item.Properties().group(ExpansionContent.expansionItemGroup).maxStackSize(1))
{
    override def addInformation(stack:ItemStack, worldIn:World, tooltip:util.List[ITextComponent], flagIn:ITooltipFlag):Unit = {
        import InfusedEnderPearlItem._
        if (hasLocation(stack)) {
            val pos = getLocation(stack)
            tooltip.add(new StringTextComponent(TextFormatting.GRAY+s"Tied to [${pos.getX}, ${pos.getY}, ${pos.getZ}]"))
        }
    }
}

object InfusedEnderPearlItem
{
    def setLocation(stack:ItemStack, pos:BlockPos):Unit = {
        setLocation(stack, pos.getX, pos.getY, pos.getZ)
    }

    def setLocation(stack:ItemStack, x:Int, y:Int, z:Int):Unit = {
        val tag = stack.getOrCreateTag()
        tag.putInt("locX", x)
        tag.putInt("locY", y)
        tag.putInt("locZ", z)
    }

    def getLocation(stack:ItemStack):BlockPos =
    {
        if (stack.hasTag) {
            val tag = stack.getTag
            new BlockPos(tag.getInt("locX"), tag.getInt("locY"), tag.getInt("locZ"))
        } else
            null
    }

    def hasLocation(stack:ItemStack) =
        stack.hasTag && stack.getTag.contains("locX")
}