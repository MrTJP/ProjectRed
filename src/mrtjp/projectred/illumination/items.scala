package mrtjp.projectred.illumination

import java.util.{List => JList}

import codechicken.lib.vec.Vector3
import codechicken.multipart.{TItemMultiPart, TMultiPart}
import mrtjp.core.item.ItemCore
import mrtjp.projectred.ProjectRedIllumination
import net.minecraft.block.SoundType
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.entity.player.EntityPlayer
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

//abstract class ItemPartButtonCommons(name:String) extends ItemCore(name) with TItemMultiPart with TItemGlassSound
//{
//    setHasSubtypes(true)
//    setCreativeTab(ProjectRedIllumination.tabLighting)
//
//    override def newPart(item:ItemStack, player:EntityPlayer, w:World, pos1:BlockCoord, side:Int, vhit:Vector3):TMultiPart =
//    {
//        if (side == 0 || side == 1) return null
//        val pos = pos1.copy.offset(side^1)
//        if (!w.isSideSolid(pos.x, pos.y, pos.z, ForgeDirection.getOrientation(side))) return null
//
//        val b = getNewInst(ButtonPart.sideMetaMap(side^1))
//        if (b != null) b.onPlaced(item)
//        b
//    }
//
//    def getNewInst(sMask:Int):LightButtonPart
//
//    override def getSubItems(item:Item, tab:CreativeTabs, list:JList[_])
//    {
//        for (i <- 0 until 16) list.asInstanceOf[JList[ItemStack]].add(new ItemStack(this, 1, i))
//    }
//
//    @SideOnly(Side.CLIENT)
//    override def getSpriteNumber = 0
//
//    @SideOnly(Side.CLIENT)
//    override def registerIcons(reg:IIconRegister) {}
//}
//
//class ItemPartButton extends ItemPartButtonCommons("projectred.illumination.lightbutton") with TItemMultiPart with TItemGlassSound
//{
//    @SideOnly(Side.CLIENT)
//    override def registerIcons(reg:IIconRegister)
//    {
//        ItemPartButton.icon = reg.registerIcon("projectred:lighting/button")
//    }
//
//    override def getNewInst(sMask:Int) = new LightButtonPart(sMask)
//}
//
//object ItemPartButton
//{
//    var icon:IIcon = null
//}
//
//class ItemPartFButton extends ItemPartButtonCommons("projectred.illumination.flightbutton")
//{
//    override def getNewInst(sMask:Int) = new FLightButtonPart(sMask)
//}