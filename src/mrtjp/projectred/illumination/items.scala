package mrtjp.projectred.illumination

import java.util.{List => JList}

import codechicken.lib.vec.{BlockCoord, Vector3}
import codechicken.multipart.minecraft.ButtonPart
import codechicken.multipart.{MultiPartRegistry, TItemMultiPart, TMultiPart}
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.projectred.ProjectRedIllumination
import mrtjp.projectred.core.libmc.WireLib
import mrtjp.projectred.core.{ItemCore, TItemGlassSound}
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.util.IIcon
import net.minecraft.world.World
import net.minecraftforge.common.util.ForgeDirection

class ItemBaseLight(obj:LightObject, val inverted:Boolean) extends ItemCore(obj.getItemName+(if (inverted) ".inv" else "")) with TItemMultiPart with TItemGlassSound
{
    setHasSubtypes(true)
    setCreativeTab(ProjectRedIllumination.tabLighting)

    override def newPart(stack:ItemStack, player:EntityPlayer, w:World, pos:BlockCoord, side:Int, vhit:Vector3):TMultiPart =
    {
        val bc = pos.copy.offset(side^1)
        if (!WireLib.canPlaceLight(w, bc.x, bc.y, bc.z, side, obj.canFloat)) return null

        val light = MultiPartRegistry.createPart(obj.getType, false).asInstanceOf[BaseLightPart]
        if (light != null) light.preparePlacement(side^1, stack.getItemDamage, inverted)
        light
    }

    @SideOnly(Side.CLIENT)
    override def getSpriteNumber = 0

    override def getSubItems(item:Item, tab:CreativeTabs, list:JList[_])
    {
        for (i <- 0 until 16) list.asInstanceOf[JList[ItemStack]].add(new ItemStack(this, 1, i))
    }

    @SideOnly(Side.CLIENT)
    override def registerIcons(reg:IIconRegister)
    {
        if (!inverted) obj.registerIcons(reg)
    }
}

abstract class ItemPartButtonCommons(name:String) extends ItemCore(name) with TItemMultiPart with TItemGlassSound
{
    setHasSubtypes(true)
    setCreativeTab(ProjectRedIllumination.tabLighting)

    override def newPart(item:ItemStack, player:EntityPlayer, w:World, pos1:BlockCoord, side:Int, vhit:Vector3):TMultiPart =
    {
        if (side == 0 || side == 1) return null
        val pos = pos1.copy.offset(side^1)
        if (!w.isSideSolid(pos.x, pos.y, pos.z, ForgeDirection.getOrientation(side))) return null

        val b = getNewInst(ButtonPart.sideMetaMap(side^1))
        if (b != null) b.onPlaced(item)
        b
    }

    def getNewInst(sMask:Int):LightButtonPart

    override def getSubItems(item:Item, tab:CreativeTabs, list:JList[_])
    {
        for (i <- 0 until 16) list.asInstanceOf[JList[ItemStack]].add(new ItemStack(this, 1, i))
    }

    @SideOnly(Side.CLIENT)
    override def getSpriteNumber = 0

    @SideOnly(Side.CLIENT)
    override def registerIcons(reg:IIconRegister) {}
}

class ItemPartButton extends ItemPartButtonCommons("projectred.illumination.lightbutton") with TItemMultiPart with TItemGlassSound
{
    @SideOnly(Side.CLIENT)
    override def registerIcons(reg:IIconRegister)
    {
        for (i <- 0 until 16)
            ItemPartButton.icons(i) = reg.registerIcon("projectred:lights/button/"+i)
    }

    override def getNewInst(sMask:Int) = new LightButtonPart(sMask)
}

object ItemPartButton
{
    val icons = new Array[IIcon](16)
}

class ItemPartFButton extends ItemPartButtonCommons("projectred.illumination.flightbutton")
{
    override def getNewInst(sMask:Int) = new FLightButtonPart(sMask)
}