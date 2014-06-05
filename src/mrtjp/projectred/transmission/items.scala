package mrtjp.projectred.transmission

import codechicken.lib.vec._
import codechicken.multipart.{TItemMultiPart, MultiPartRegistry}
import cpw.mods.fml.relauncher.{SideOnly, Side}
import mrtjp.projectred.ProjectRedTransmission
import mrtjp.projectred.core.{TItemGlassSound, ItemCore}
import net.minecraft.block.Block
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.world.World
import java.util.{List => JList}
import net.minecraftforge.client.IItemRenderer
import net.minecraftforge.client.IItemRenderer.ItemRendererHelper
import codechicken.lib.render.TextureUtils
import net.minecraft.util.IIcon
import mrtjp.projectred.core.libmc.BasicWireUtils
import net.minecraftforge.common.util.ForgeDirection

abstract class ItemWireCommon(name:String) extends ItemCore(name) with TItemMultiPart with TItemGlassSound
{
    setHasSubtypes(true)
    setCreativeTab(ProjectRedTransmission.tabTransmission)

    @SideOnly(Side.CLIENT)
    override def getSpriteNumber = 0
}

object ItemPartWire
{
    var additionalWires = Seq[ItemStack]()
}

class ItemPartWire extends ItemWireCommon("projectred.transmission.wire")
{
    def newPart(item:ItemStack, player:EntityPlayer, world:World, pos:BlockCoord, side:Int, vhit:Vector3) =
    {
        val onPos = pos.copy.offset(side^1)
        if (!BasicWireUtils.canPlaceWireOnSide(world, onPos.x, onPos.y, onPos.z, ForgeDirection.getOrientation(side), false)) null
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

        for (w <- WireDef.RED_ALLOY++WireDef.INSULATED_WIRES++WireDef.BUNDLED_WIRES)
            if (w.hasWireForm) l2.add(w.makeStack)

        for (w <- ItemPartWire.additionalWires) l2.add(w)
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

        for (w <- WireDef.RED_ALLOY++WireDef.INSULATED_WIRES++WireDef.BUNDLED_WIRES)
            if (w.hasFramedForm) l2.add(w.makeFramedStack)

        for (w <- ItemPartFramedWire.additionalWires) l2.add(w)
    }
}

object ItemPartFramedWire
{
    var additionalWires = Seq[ItemStack]()
}