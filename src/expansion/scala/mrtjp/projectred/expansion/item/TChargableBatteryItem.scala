package mrtjp.projectred.expansion.item

import net.minecraft.item.{Item, ItemStack}

trait TChargableBatteryItem extends IChargable
{
    def isEmpty:Boolean

    def nonEmpty:Boolean = !isEmpty

    def getEmptyVariant:Item

    def getChargedVariant:Item

    override def addPower(stack:ItemStack, pow:Int):(ItemStack, Int) = stack.getItem match {
        case b:TChargableBatteryItem if pow > 0 =>
            val newStack = if (b.isEmpty) {
                val s = new ItemStack(b.getChargedVariant, 1)
                s.setDamageValue(s.getMaxDamage)
                s
            } else stack

            val spaceLeft = newStack.getDamageValue
            val toAdd = Math.min(spaceLeft, pow)
            newStack.setDamageValue(newStack.getDamageValue - toAdd)
            (newStack, toAdd)
        case _ => (stack, 0)
    }

    override def drawPower(stack:ItemStack, pow:Int):(ItemStack, Int) = stack.getItem match {
        case b:TChargableBatteryItem if b.nonEmpty =>
            val powerLeft = stack.getMaxDamage - stack.getDamageValue
            val toDraw = Math.min(powerLeft, pow)
            stack.setDamageValue(stack.getDamageValue + toDraw)
            val newStack = if (stack.getDamageValue >= stack.getMaxDamage) new ItemStack(b.getEmptyVariant) else stack
            (newStack, toDraw)
        case _ => (stack, 0)
    }

    override def isFullyCharged(stack:ItemStack):Boolean =
        stack.getDamageValue == 0 && stack.getItem == getChargedVariant
}
