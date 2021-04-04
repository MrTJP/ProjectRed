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
                s.setDamage(s.getMaxDamage)
                s
            } else stack

            val spaceLeft = newStack.getDamage
            val toAdd = Math.min(spaceLeft, pow)
            newStack.setDamage(newStack.getDamage - toAdd)
            (newStack, toAdd)
        case _ => (stack, 0)
    }

    override def drawPower(stack:ItemStack, pow:Int):(ItemStack, Int) = stack.getItem match {
        case b:TChargableBatteryItem if b.nonEmpty =>
            val powerLeft = stack.getMaxDamage - stack.getDamage
            val toDraw = Math.min(powerLeft, pow)
            stack.setDamage(stack.getDamage + toDraw)
            val newStack = if (stack.getDamage >= stack.getMaxDamage) new ItemStack(b.getEmptyVariant) else stack
            (newStack, toDraw)
        case _ => (stack, 0)
    }

    override def isFullyCharged(stack:ItemStack):Boolean =
        stack.getDamage == 0 && stack.getItem == getChargedVariant
}
