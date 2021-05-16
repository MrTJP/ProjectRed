package mrtjp.projectred.expansion.item

import net.minecraft.enchantment.Enchantment
import net.minecraft.item.ItemStack

trait IChargable
{
    def addPower(stack:ItemStack, pow:Int):(ItemStack, Int) =
        stack.getItem match {
            case b:IChargable if pow > 0 =>
                val spaceLeft = stack.getDamageValue
                val toAdd = Math.min(spaceLeft, pow)
                stack.setDamageValue(stack.getDamageValue - toAdd)
                (stack, toAdd)
            case _ => (stack, 0)
        }

    def drawPower(stack:ItemStack, pow:Int):(ItemStack, Int) =
        stack.getItem match {
            case b:IChargable =>
                val powerLeft = stack.getMaxDamage - stack.getDamageValue
                val toDraw = Math.min(powerLeft, pow)
                stack.setDamageValue(stack.getDamageValue + toDraw)
                (stack, toDraw)
            case _ => (stack, 0)
        }

    def isFullyCharged(stack:ItemStack):Boolean = stack.getDamageValue == 0

    def canApplyElectricEnchantment(enchantment:Enchantment) = false
}
