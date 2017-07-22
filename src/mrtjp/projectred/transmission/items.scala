package mrtjp.projectred.transmission

import java.util.{List => JList}

import codechicken.lib.vec._
import codechicken.multipart.{MultiPartRegistry, TItemMultiPart}
import mrtjp.core.item.ItemCore
import mrtjp.projectred.ProjectRedTransmission
import mrtjp.projectred.core.PRLib
import net.minecraft.block.SoundType
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.util.math.BlockPos
import net.minecraft.util.{EnumFacing, NonNullList}
import net.minecraft.world.World
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

class ItemPartWire extends ItemCore with TItemMultiPart
{
    setHasSubtypes(true)
    setCreativeTab(ProjectRedTransmission.tabTransmission)

    def newPart(item:ItemStack, player:EntityPlayer, world:World, pos:BlockPos, side:Int, vhit:Vector3) =
    {
        val onPos = pos.offset(EnumFacing.values()(side^1))
        if (!PRLib.canPlaceWireOnSide(world, onPos, side)) null
        else {
            val wiredef = WireDef.values(item.getItemDamage)
            val w = MultiPartRegistry.loadPart(wiredef.wireType, null).asInstanceOf[WirePart]
            if (w != null) w.preparePlacement(side, item.getItemDamage)
            w
        }
    }

    @SideOnly(Side.CLIENT)
    override def getSubItems(i:Item, tab:CreativeTabs, list:NonNullList[ItemStack])
    {
        for (w <- WireDef.values)
            if (w.hasWireForm) list.add(w.makeStack)
    }

    override def getPlacementSound(item:ItemStack) = SoundType.GLASS
}

class ItemPartFramedWire extends ItemCore with TItemMultiPart
{
    setHasSubtypes(true)
    setCreativeTab(ProjectRedTransmission.tabTransmission)

    def newPart(item:ItemStack, player:EntityPlayer, world:World, pos:BlockPos, side:Int, vhit:Vector3) =
    {
        val wiredef = WireDef.values(item.getItemDamage)
        val w = MultiPartRegistry.loadPart(wiredef.framedType, null).asInstanceOf[FramedWirePart]
        if (w != null) w.preparePlacement(side, item.getItemDamage)
        w
    }

    @SideOnly(Side.CLIENT)
    override def getSubItems(i:Item, tab:CreativeTabs, list:NonNullList[ItemStack])
    {
        for (w <- WireDef.values)
            if (w.hasFramedForm) list.add(w.makeFramedStack)
    }

    override def getPlacementSound(item:ItemStack) = SoundType.GLASS
}