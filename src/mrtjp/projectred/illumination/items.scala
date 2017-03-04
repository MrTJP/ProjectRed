package mrtjp.projectred.illumination

import java.util.{List => JList}

import codechicken.lib.vec.Vector3
import codechicken.multipart.{TItemMultiPart, TMultiPart}
import mrtjp.core.item.ItemCore
import mrtjp.projectred.ProjectRedIllumination
import net.minecraft.block.SoundType
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.init.Blocks
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.util.EnumFacing
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World

class ItemBaseLight(factory:LightFactory, val inverted:Boolean) extends ItemCore with TItemMultiPart
{
    setHasSubtypes(true)
    setCreativeTab(ProjectRedIllumination.tabLighting)

    override def newPart(stack:ItemStack, player:EntityPlayer, w:World, pos:BlockPos, side:Int, vhit:Vector3):TMultiPart =
    {
        val bc = pos.offset(EnumFacing.values()(side^1))
        if (!factory.canFloat && !BaseLightPart.canPlaceLight(w, bc, side)) return null

        val light = factory.createPart

        if (light != null)
            light.preparePlacement(side^1, stack.getItemDamage, inverted)

        light
    }

    override def getPlacementSound(item:ItemStack) = SoundType.GLASS

    override def getSubItems(item:Item, tab:CreativeTabs, list:JList[ItemStack])
    {
        for (i <- 0 until 16) list.add(new ItemStack(this, 1, i))
    }
}

abstract class ItemPartButtonCommons extends ItemCore with TItemMultiPart
{
    setHasSubtypes(true)
    setCreativeTab(ProjectRedIllumination.tabLighting)

    /**
      * Create a new part based on the placement information parameters.
      */
    override def newPart(item:ItemStack, player:EntityPlayer, world:World, pos:BlockPos, side:Int, vhit:Vector3):TMultiPart =
    {
        val pos2 = pos.offset(EnumFacing.values()(side^1))
        if (!world.isSideSolid(pos2, EnumFacing.values()(side))) return null


        val b = getNewInst
        if (b != null)
            b.setStateOnPlacement(world, pos, EnumFacing.values()(side), vhit.vec3(), player, item)
        b
    }

    def getNewInst:LightButtonPart

    override def getSubItems(itemIn:Item, tab:CreativeTabs, subItems:JList[ItemStack]) =
    {
        for (i <- 0 until 16) subItems.add(new ItemStack(this, 1, i))
    }

    override def getPlacementSound(item:ItemStack):SoundType = SoundType.GLASS

}

class ItemPartButton extends ItemPartButtonCommons
{
    override def getNewInst = new LightButtonPart
}

class ItemPartFButton extends ItemPartButtonCommons
{
    override def getNewInst = new FLightButtonPart
}