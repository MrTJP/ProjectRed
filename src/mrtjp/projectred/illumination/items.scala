package mrtjp.projectred.illumination

import net.minecraft.item.{ItemStack, Item}
import codechicken.multipart.{TMultiPart, MultiPartRegistry, TItemMultiPart}
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.world.World
import codechicken.lib.vec.{Vector3, BlockCoord}
import mrtjp.projectred.ProjectRedIllumination
import mrtjp.projectred.core.{TItemGlassSound, ItemCore}
import cpw.mods.fml.relauncher.{SideOnly, Side}
import net.minecraft.creativetab.CreativeTabs
import java.util.{List => JList}
import mrtjp.projectred.core.libmc.WireLib
import net.minecraftforge.common.util.ForgeDirection
import codechicken.multipart.minecraft.ButtonPart
import net.minecraft.util.IIcon
import net.minecraft.client.renderer.texture.IIconRegister

class ItemBaseLight(obj:LightObject, val inverted:Boolean) extends ItemCore(obj.getItemName+(if (inverted) ".inv" else "")) with TItemMultiPart with TItemGlassSound
{
    setHasSubtypes(true)
    setCreativeTab(ProjectRedIllumination.tabLighting)

    override def newPart(stack:ItemStack, player:EntityPlayer, w:World, pos:BlockCoord, side:Int, vhit:Vector3):TMultiPart =
    {
        val bc = pos.copy.offset(side^1)
        if (!WireLib.canPlaceWireOnSide(w, bc.x, bc.y, bc.z, ForgeDirection.getOrientation(side), false) && !(WireLib.canPlaceTorchOnBlock(w, bc.x, bc.y, bc.z, false) && side == 1)) return null

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