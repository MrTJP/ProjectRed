package mrtjp.projectred.integration

import mrtjp.projectred.core.ItemCore
import codechicken.multipart.{MultiPartRegistry, TMultiPart, TItemMultiPart}
import mrtjp.projectred.ProjectRedIntegration
import net.minecraft.item.ItemStack
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.world.World
import net.minecraft.block.Block
import codechicken.lib.vec.{Translation, Scale, Vector3, BlockCoord}
import mrtjp.projectred.core.libmc.BasicWireUtils
import net.minecraftforge.common.util.ForgeDirection
import net.minecraft.creativetab.CreativeTabs
import java.util.{List => JList}

import cpw.mods.fml.relauncher.{SideOnly, Side}
import net.minecraftforge.client.IItemRenderer
import net.minecraftforge.client.IItemRenderer.{ItemRendererHelper, ItemRenderType}
import codechicken.lib.render.{CCRenderState, TextureUtils}

class ItemPartGate extends ItemCore("projectred.integration.gate") with TItemMultiPart
{
    setHasSubtypes(true)
    setCreativeTab(ProjectRedIntegration.tabIntegration)

    override def onItemUse(stack:ItemStack, player:EntityPlayer, w:World, x:Int, y:Int, z:Int, side:Int, f:Float, f2:Float, f3:Float) =
    {
        if (super.onItemUse(stack, player, w, x, y, z, side, f, f2, f3))
        {
            w.playSoundEffect(x+0.5, y+0.5, z+0.5, Block.soundTypeGlass.func_150496_b(),
                Block.soundTypeGlass.getVolume*5.0F, Block.soundTypeGlass.getPitch*.9F)
            true
        }
        else false
    }

    def newPart(item:ItemStack, player:EntityPlayer, world:World, pos:BlockCoord, side:Int, vhit:Vector3):TMultiPart =
    {
        val onPos = pos.copy.offset(side^1)
        if (!BasicWireUtils.canPlaceWireOnSide(world, onPos.x, onPos.y, onPos.z, ForgeDirection.getOrientation(side), false)) return null

        val gtype = EnumGate.VALID_GATES(item.getItemDamage)
        if (!gtype.implemented) return null

        val gate = MultiPartRegistry.createPart(gtype.gateType, false).asInstanceOf[GatePart]
        if (gate != null) gate.preparePlacement(player, pos, side, item.getItemDamage)
        gate
    }

    @SideOnly(Side.CLIENT)
    def getSubItems(id:Int, tab:CreativeTabs, list:JList[_])
    {
        val l2 = list.asInstanceOf[JList[ItemStack]]
        for (g <- EnumGate.VALID_GATES) l2.add(g.makeStack)
    }

    def registerIcons(reg:Nothing)
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
        CCRenderState.useNormals = true
        CCRenderState.pullLightmap()
        CCRenderState.setColour(-1)
        RenderGate.renderInv(new Scale(scale).`with`(new Translation(x, y, z)), meta)
    }
}