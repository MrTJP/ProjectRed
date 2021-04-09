package mrtjp.projectred.expansion.item

import net.minecraft.enchantment.Enchantment
import net.minecraft.item.ItemStack

trait IChargable
{
    def addPower(stack:ItemStack, pow:Int):(ItemStack, Int) =
        stack.getItem match {
            case b:IChargable if pow > 0 =>
                val spaceLeft = stack.getDamage
                val toAdd = Math.min(spaceLeft, pow)
                stack.setDamage(stack.getDamage - toAdd)
                (stack, toAdd)
            case _ => (stack, 0)
        }

    def drawPower(stack:ItemStack, pow:Int):(ItemStack, Int) =
        stack.getItem match {
            case b:IChargable =>
                val powerLeft = stack.getMaxDamage - stack.getDamage
                val toDraw = Math.min(powerLeft, pow)
                stack.setDamage(stack.getDamage + toDraw)
                (stack, toDraw)
            case _ => (stack, 0)
        }

    def isFullyCharged(stack:ItemStack):Boolean = stack.getDamage == 0

    def canApplyElectricEnchantment(enchantment:Enchantment) = false
}
