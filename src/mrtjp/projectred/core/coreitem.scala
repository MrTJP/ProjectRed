package mrtjp.projectred.core

import net.minecraft.item.{ItemStack, Item}
import cpw.mods.fml.common.registry.GameRegistry
import mrtjp.projectred.core.lib.Enum
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.world.World
import net.minecraft.block.Block

class ItemCore(name:String) extends Item
{
    setUnlocalizedName(name)
    GameRegistry.registerItem(this, name)

    override def getUnlocalizedName(stack:ItemStack):String =
        if (hasSubtypes) getUnlocalizedName()+"|"+stack.getItemDamage
        else getUnlocalizedName()
}

trait TItemGlassSound extends Item
{
    abstract override def onItemUse(stack:ItemStack, player:EntityPlayer, w:World, x:Int, y:Int, z:Int, side:Int, f:Float, f2:Float, f3:Float) =
    {
        if (super.onItemUse(stack, player, w, x, y, z, side, f, f2, f3))
        {
            w.playSoundEffect(x+0.5, y+0.5, z+0.5, Block.soundTypeGlass.func_150496_b(),
                Block.soundTypeGlass.getVolume*5.0F, Block.soundTypeGlass.getPitch*.9F)
            true
        }
        else false
    }
}

/**
 * Object that collects defs for all subtypes of this item if it has any.
 * Extend ItemDefinition as enum object.
 */
abstract class ItemDefinition extends Enum
{
    type EnumVal <: ItemDef

    def getItem:Item

    /**
     *
     * Define items here
     *
     */

    class ItemDef extends Value
    {
        val meta = ordinal

        override def name = getItem.getUnlocalizedName(makeStack)

        def makeStack:ItemStack = makeStack(1)
        def makeStack(i:Int) = new ItemStack(getItem, i, meta)
    }
}

