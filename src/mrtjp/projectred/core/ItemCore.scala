package mrtjp.projectred.core

import net.minecraft.item.{ItemStack, Item}
import cpw.mods.fml.common.registry.GameRegistry
import mrtjp.projectred.core.lib.Enum

class ItemCore(name:String) extends Item
{
    setUnlocalizedName(name)
    GameRegistry.registerItem(this, name)

    override def getUnlocalizedName(stack:ItemStack) =
        if (hasSubtypes) getUnlocalizedName+"|"+stack.getItemDamage
        else getUnlocalizedName
}

/**
 * Object that collects info for all subtypes of this item if it has any.
 * Extend enum as object.
 */
abstract class ItemCollection extends Enum
{
    type EnumVal = ItemColVal

    def getItem:Item

    /**
     *
     * Define items here
     *
     */

    class ItemColVal extends Value
    {
        val meta = ordinal

        override def name = getItem.getUnlocalizedName(makeStack)

        def makeStack:ItemStack = makeStack(1)
        def makeStack(i:Int) = new ItemStack(getItem, i, meta)
    }
}
