package mrtjp.projectred.expansion.item

import mrtjp.projectred.expansion.ExpansionContent
import net.minecraft.client.util.ITooltipFlag
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.nbt.CompoundNBT
import net.minecraft.util.text.{ITextComponent, StringTextComponent, TextFormatting}
import net.minecraft.world.World

import java.util

class PlanItem extends Item(new Item.Properties().group(ExpansionContent.expansionItemGroup))
{
    override def addInformation(stack:ItemStack, worldIn:World, tooltip:util.List[ITextComponent], flagIn:ITooltipFlag):Unit = {
        if (PlanItem.hasRecipeInside(stack)) {
            val s = s"${TextFormatting.BLUE}Output: ${TextFormatting.GRAY+PlanItem.loadPlanOutput(stack).getDisplayName.getFormattedText}"
            tooltip.add(new StringTextComponent(s))
        }
    }
}

object PlanItem
{
    def hasRecipeInside(stack:ItemStack):Boolean =
        stack.hasTag && stack.getTag.contains("planInputs") && stack.getTag.contains("planOutput")

    def savePlan(stack:ItemStack, inputs:Array[ItemStack], output:ItemStack):Unit = {
        val inputsNBT = new CompoundNBT
        for (i <- 0 until 9) {
            var slotStack = inputs(i)
            if (!slotStack.isEmpty) {
                val itemStackNBT = new CompoundNBT
                if (slotStack.isDamageable) { //save without damage bar
                    slotStack = slotStack.copy
                    slotStack.setDamage(0)
                }
                slotStack.write(itemStackNBT)
                inputsNBT.put(s"input_$i", itemStackNBT)
            }
        }
        val outputNBT = new CompoundNBT
        output.write(outputNBT)

        stack.getOrCreateTag().put("planInputs", inputsNBT)
        stack.getOrCreateTag().put("planOutput", outputNBT)
    }

    def loadPlanInputs(stack:ItemStack):Array[ItemStack] = {
        val out = Array.fill[ItemStack](9)(ItemStack.EMPTY)
        if (stack.hasTag) {
            val inputsNBT = stack.getTag.getCompound("planInputs")
            for (i <- 0 until 9) {
                val itemStackNBT = inputsNBT.getCompound(s"input_$i")
                val itemStack = ItemStack.read(itemStackNBT)
                if (!itemStack.isEmpty)
                    out(i) = itemStack
            }
        }
        out
    }

    def loadPlanOutput(stack:ItemStack):ItemStack = {
        if (stack.hasTag) {
            val outputNBT = stack.getTag.getCompound("planOutput")
            val itemStack = ItemStack.read(outputNBT)
            if (!itemStack.isEmpty) itemStack else ItemStack.EMPTY
        } else
            ItemStack.EMPTY
    }
}