package mrtjp.projectred.integration

import mrtjp.projectred.core.{TItemGlassSound, ItemCore}
import codechicken.multipart.{MultiPartRegistry, TMultiPart, TItemMultiPart}
import mrtjp.projectred.ProjectRedIntegration
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.world.World
import codechicken.lib.vec.{Translation, Scale, Vector3, BlockCoord}
import mrtjp.projectred.core.libmc.WireLib
import net.minecraftforge.common.util.ForgeDirection
import net.minecraft.creativetab.CreativeTabs
import java.util.{List => JList}

import cpw.mods.fml.relauncher.{SideOnly, Side}
import net.minecraftforge.client.IItemRenderer
import net.minecraftforge.client.IItemRenderer.{ItemRendererHelper, ItemRenderType}
import codechicken.lib.render.{CCRenderState, TextureUtils}
import net.minecraft.client.renderer.texture.IIconRegister

class ItemPartGate extends ItemCore("projectred.integration.gate") with TItemMultiPart with TItemGlassSound
{
    setHasSubtypes(true)
    setCreativeTab(ProjectRedIntegration.tabIntegration)

    def newPart(item:ItemStack, player:EntityPlayer, world:World, pos:BlockCoord, side:Int, vhit:Vector3):TMultiPart =
    {
        val onPos = pos.copy.offset(side^1)
        if (!WireLib.canPlaceWireOnSide(world, onPos.x, onPos.y, onPos.z, ForgeDirection.getOrientation(side), false)) return null

        val gtype = EnumGate.VALID_GATES(item.getItemDamage)
        if (!gtype.implemented) return null

        val gate = MultiPartRegistry.createPart(gtype.gateType, false).asInstanceOf[GatePart]
        if (gate != null) gate.preparePlacement(player, pos, side, item.getItemDamage)
        gate
    }

    @SideOnly(Side.CLIENT)
    override def getSubItems(id:Item, tab:CreativeTabs, list:JList[_])
    {
        val l2 = list.asInstanceOf[JList[ItemStack]]
        for (g <- EnumGate.VALID_GATES) l2.add(g.makeStack)
    }

    override def registerIcons(reg:IIconRegister)
    {
        ComponentStore.registerIcons(reg)
    }

    @SideOnly(Side.CLIENT)
    override def getSpriteNumber = 0
}

object GateItemRenderer extends IItemRenderer
{
    override def shouldUseRenderHelper(t:ItemRenderType, item:ItemStack, helper:ItemRendererHelper) = true

    override def handleRenderType(item:ItemStack, t:ItemRenderType) = true

    override def renderItem(t:ItemRenderType, item:ItemStack, data:AnyRef*) =
    {
        val damage = item.getItemDamage
        import ItemRenderType._
        t match
        {
            case ENTITY => renderGateInv(damage, -0.3F, 0F, -0.3F, 0.6F)
            case EQUIPPED => renderGateInv(damage, 0.0F, 0.15F, 0.0F, 1.0F)
            case EQUIPPED_FIRST_PERSON => renderGateInv(damage, 1.0F, -0.2F, -0.4f, 2.0F)
            case INVENTORY => renderGateInv(damage, 0.0F, 0.2F, 0.0F, 1.0F)
            case _ =>
        }
    }

    def renderGateInv(meta:Int, x:Float, y:Float, z:Float, scale:Float)
    {
        if (!EnumGate.VALID_GATES(meta).implemented) return
        TextureUtils.bindAtlas(0)
        CCRenderState.reset()
        CCRenderState.setDynamic()
        CCRenderState.pullLightmap()
        RenderGate.renderInv(new Scale(scale).`with`(new Translation(x, y, z)), meta)
    }
}