/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import mrtjp.core.item.ItemCore
import mrtjp.projectred.ProjectRedExpansion
import net.minecraft.item.{ItemStack, Item}

trait TItemBattery extends Item
{
    def isEmpty:Boolean
    def nonEmpty = !isEmpty

    def getEmptyVariant:Item
    def getChargedVariant:Item
}

object TItemBattery
{
    def addPower(stack:ItemStack, pow:Int):(ItemStack, Int) =
    {
        stack.getItem match
        {
            case b:TItemBattery if pow > 0 =>
                val newStack = if (b.isEmpty) new ItemStack(b.getChargedVariant, 1, b.getChargedVariant.getMaxDamage) else stack
                val spaceLeft = newStack.getItemDamage
                val toAdd = math.min(spaceLeft, pow)
                newStack.setItemDamage(newStack.getItemDamage-toAdd)
                (newStack, toAdd)
            case _ => (stack, 0)
        }
    }

    def drawPower(stack:ItemStack, pow:Int):(ItemStack, Int) =
    {
        stack.getItem match
        {
            case b:TItemBattery if b.nonEmpty =>
                val powerLeft = stack.getMaxDamage-stack.getItemDamage
                val toDraw = math.min(powerLeft, pow)
                stack.setItemDamage(stack.getItemDamage+toDraw)
                val newStack = if (stack.getItemDamage >= stack.getMaxDamage) new ItemStack(b.getEmptyVariant) else stack
                (newStack, toDraw)
            case _ => (stack, 0)
        }
    }
}

class ItemBatteryEmpty extends ItemCore("projectred.expansion.emptybattery") with TItemBattery
{
    setCreativeTab(ProjectRedExpansion.tabExpansion)
    setTextureName("projectred:emptybattery")

    override def isEmpty = true

    override def getEmptyVariant = this
    override def getChargedVariant = ProjectRedExpansion.battery
}

class ItemBattery extends ItemCore("projectred.expansion.battery") with TItemBattery
{
    setMaxDamage(1600)
    setNoRepair()
    setMaxStackSize(1)
    setTextureName("projectred:battery")
    setCreativeTab(ProjectRedExpansion.tabExpansion)

    override def isEmpty = false

    override def getEmptyVariant = ProjectRedExpansion.emptybattery
    override def getChargedVariant = this
}