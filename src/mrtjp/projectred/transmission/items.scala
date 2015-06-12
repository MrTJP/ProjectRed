package mrtjp.projectred.transmission

import java.util.{List => JList}

import codechicken.lib.vec._
import codechicken.multipart.{MultiPartRegistry, TItemMultiPart}
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.item.{ItemCore, TItemGlassSound}
import mrtjp.core.world.PlacementLib
import mrtjp.projectred.ProjectRedTransmission
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.world.World

abstract class ItemWireCommon(name:String) extends ItemCore(name) with TItemMultiPart with TItemGlassSound
{
    setHasSubtypes(true)
    setCreativeTab(ProjectRedTransmission.tabTransmission)

    @SideOnly(Side.CLIENT)
    override def getSpriteNumber = 0

    override def registerIcons(reg:IIconRegister){}
}

class ItemPartWire extends ItemWireCommon("projectred.transmission.wire")
{
    def newPart(item:ItemStack, player:EntityPlayer, world:World, pos:BlockCoord, side:Int, vhit:Vector3) =
    {
        val onPos = pos.copy.offset(side^1)
        if (!PlacementLib.canPlaceWireOnSide(world, onPos.x, onPos.y, onPos.z, side)) null
        else
        {
            val wiredef = WireDef.values(item.getItemDamage)
            val w = MultiPartRegistry.createPart(wiredef.wireType, false).asInstanceOf[WirePart]
            if (w != null) w.preparePlacement(side, item.getItemDamage)
            w
        }
    }

    @SideOnly(Side.CLIENT)
    override def getSubItems(i:Item, tab:CreativeTabs, list:JList[_])
    {
        val l2 = list.asInstanceOf[JList[ItemStack]]

        for (w <- WireDef.values)
            if (w.hasWireForm) l2.add(w.makeStack)
    }

    @SideOnly(Side.CLIENT)
    override def registerIcons(reg:IIconRegister)
    {
        for (w <- WireDef.values) w.loadTextures(reg)
    }
}

class ItemPartFramedWire extends ItemWireCommon("projectred.transmission.framewire")
{
    def newPart(item:ItemStack, player:EntityPlayer, world:World, pos:BlockCoord, side:Int, vhit:Vector3) =
    {
        val wiredef = WireDef.values(item.getItemDamage)
        val w = MultiPartRegistry.createPart(wiredef.framedType, false).asInstanceOf[FramedWirePart]
        if (w != null) w.preparePlacement(side, item.getItemDamage)
        w
    }

    @SideOnly(Side.CLIENT)
    override def getSubItems(i:Item, tab:CreativeTabs, list:JList[_])
    {
        val l2 = list.asInstanceOf[JList[ItemStack]]

        for (w <- WireDef.values)
            if (w.hasFramedForm) l2.add(w.makeFramedStack)
    }
}