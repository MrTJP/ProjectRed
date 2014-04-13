package mrtjp.projectred.transmission

import codechicken.lib.vec._
import codechicken.multipart.{TileMultipart, TMultiPart, TItemMultiPart, MultiPartRegistry}
import cpw.mods.fml.relauncher.{SideOnly, Side}
import mrtjp.projectred.ProjectRedTransmission
import mrtjp.projectred.core.{BasicUtils, BasicWireUtils}
import net.minecraft.block.Block
import net.minecraft.client.renderer.texture.IconRegister
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.world.World
import net.minecraftforge.common.ForgeDirection
import java.util.{List => JList}
import net.minecraftforge.client.IItemRenderer
import net.minecraftforge.client.IItemRenderer.{ItemRenderType, ItemRendererHelper}
import codechicken.lib.render.{CCRenderState, TextureUtils}
import net.minecraft.util.{MovingObjectPosition, Icon}
import scala.collection.convert.WrapAsScala
import scala.collection.mutable
import codechicken.microblock.MicroMaterialRegistry.IMicroHighlightRenderer
import codechicken.microblock.{MicroblockClass, MicroMaterialRegistry}
import codechicken.lib.raytracer.ExtendedMOP

abstract class ItemWireCommon(id:Int) extends Item(id) with TItemMultiPart
{
    setHasSubtypes(true)
    setCreativeTab(ProjectRedTransmission.tabTransmission)

    override def onItemUse(stack:ItemStack, player:EntityPlayer, w:World, x:Int, y:Int, z:Int, side:Int, f:Float, f2:Float, f3:Float) =
    {
        if (super.onItemUse(stack, player, w, x, y, z, side, f, f2, f3))
        {
            w.playSoundEffect(x+0.5, y+0.5, z+0.5, Block.soundGlassFootstep.getPlaceSound,
                Block.soundGlassFootstep.getVolume*5.0F, Block.soundGlassFootstep.getPitch*.9F)
            true
        }
        else false
    }

    override def getUnlocalizedName(stack:ItemStack) = super.getUnlocalizedName+"|"+stack.getItemDamage

    @SideOnly(Side.CLIENT)
    override def getSpriteNumber = 0
}

object ItemPartWire
{
    var additionalWires = Seq[ItemStack]()
}

class ItemPartWire(id:Int) extends ItemWireCommon(id)
{
    setUnlocalizedName("projectred.transmission.wire")

    def newPart(item:ItemStack, player:EntityPlayer, world:World, pos:BlockCoord, side:Int, vhit:Vector3) =
    {
        val onPos = pos.copy.offset(side^1)
        if (!BasicWireUtils.canPlaceWireOnSide(world, onPos.x, onPos.y, onPos.z, ForgeDirection.getOrientation(side), false)) null
        else
        {
            val wiredef = WireDef.VALID_WIRE(item.getItemDamage)
            val w = MultiPartRegistry.createPart(wiredef.wireType, false).asInstanceOf[WirePart]
            if (w != null) w.preparePlacement(side, item.getItemDamage)
            w
        }
    }

    @SideOnly(Side.CLIENT)
    override def getSubItems(id:Int, tab:CreativeTabs, list:JList[_])
    {
        val l2 = list.asInstanceOf[JList[ItemStack]]

        for (w <- Seq(WireDef.RED_ALLOY)++WireDef.INSULATED_WIRE++WireDef.BUNDLED_WIRE)
            if (w.hasWireForm) l2.add(w.getItemStack)

        for (w <- ItemPartWire.additionalWires) l2.add(w)
    }

    @SideOnly(Side.CLIENT)
    override def registerIcons(reg:IconRegister)
    {
        for (w <- WireDef.VALID_WIRE) w.loadTextures(reg)
    }
}



class ItemPartFramedWire(id:Int) extends ItemWireCommon(id)
{
    setUnlocalizedName("projectred.transmission.framewire")

    def newPart(item:ItemStack, player:EntityPlayer, world:World, pos:BlockCoord, side:Int, vhit:Vector3) =
    {
        val wiredef = WireDef.VALID_WIRE(item.getItemDamage)
        val w = MultiPartRegistry.createPart(wiredef.framedType, false).asInstanceOf[FramedWirePart]
        if (w != null) w.preparePlacement(side, item.getItemDamage)
        w
    }

    @SideOnly(Side.CLIENT)
    override def getSubItems(id:Int, tab:CreativeTabs, list:JList[_])
    {
        val l2 = list.asInstanceOf[JList[ItemStack]]

        for (w <- Seq(WireDef.RED_ALLOY)++WireDef.INSULATED_WIRE++WireDef.BUNDLED_WIRE)
            if (w.hasFramedForm) l2.add(w.getFramedItemStack)

        for (w <- ItemPartFramedWire.additionalWires) l2.add(w)
    }
}

object ItemPartFramedWire
{
    var additionalWires = Seq[ItemStack]()
}

trait TWireItemRenderCommon extends IItemRenderer
{
    def handleRenderType(item:ItemStack, rtype:ItemRenderType) = true

    def shouldUseRenderHelper(rtype:ItemRenderType, item:ItemStack, helper:ItemRendererHelper) = true

    def renderWireInventory(meta:Int, x:Float, y:Float, z:Float, scale:Float)
    {
        val wdef = WireDef.VALID_WIRE(meta)
        if (wdef == null) return
        TextureUtils.bindAtlas(0)
        CCRenderState.reset()
        CCRenderState.useNormals(true)
        CCRenderState.pullLightmap()
        CCRenderState.setColourOpaque(wdef.itemColour)
        CCRenderState.startDrawing(7)

//        RenderFramedWire.renderInv(wdef.thickness, new Scale(scale).`with`(new Translation(x, y, z)), wdef.wireSprites(0))
        doRender(wdef.thickness, new Scale(scale).`with`(new Translation(x, y, z)), wdef.wireSprites(0))

        CCRenderState.draw()
    }

    def doRender(thickness:Int, t:Transformation, icon:Icon)
}

object WireItemRenderer extends TWireItemRenderCommon
{
    override def renderItem(rtype:ItemRenderType, item:ItemStack, data:AnyRef*)
    {
        val damage = item.getItemDamage
        import ItemRenderType._
        rtype match
        {
            case ENTITY => renderWireInventory(damage, -0.3f, 0f, -0.3f, 0.6f)
            case EQUIPPED => renderWireInventory(damage, 0f, .0f, 0f, 1f)
            case EQUIPPED_FIRST_PERSON => renderWireInventory(damage, 1f, -0.6f, -0.4f, 2f)
            case INVENTORY => renderWireInventory(damage, 0f, -0.1f, 0f, 1f)
            case _ =>
        }
    }

    override def doRender(thickness:Int, t:Transformation, icon:Icon)
    {
        RenderWire.renderInv(thickness, t, icon)
    }
}

object FramedWireItemRenderer extends TWireItemRenderCommon
{
    override def renderItem(rtype:ItemRenderType, item:ItemStack, data:AnyRef*)
    {
        val damage = item.getItemDamage
        import ItemRenderType._
        rtype match
        {
            case ENTITY => renderWireInventory(damage, -0.5f, 0f, -0.5f, 1f)
            case EQUIPPED => renderWireInventory(damage, 0f, 0f, 0f, 1f)
            case EQUIPPED_FIRST_PERSON => renderWireInventory(damage, 1f, -0.6f, -0.4f, 2f)
            case INVENTORY => renderWireInventory(damage, 0f, -0.1f, 0f, 1f)
            case _ =>
        }
    }

    override def doRender(thickness:Int, t:Transformation, icon:Icon)
    {
        RenderFramedWire.renderInv(thickness, t, icon)
    }
}

object JacketedHighlightRenderer extends IMicroHighlightRenderer
{
    def renderHighlight(world:World, player:EntityPlayer, hit:MovingObjectPosition, mcrClass:MicroblockClass, size:Int, material:Int) =
    {
        val tile = BasicUtils.getMultipartTile(world, new BlockCoord(hit.blockX, hit.blockY, hit.blockZ))
        if (tile == null || mcrClass.classID != 0 || size != 1 || player.isSneaking ||
            MicroMaterialRegistry.getMaterial(material).isTransparent) false
        else
        {
            val hitData:(Integer, Any) = ExtendedMOP.getData(hit)
            val part = tile.partList(hitData._1)

            part match
            {
                case fpart:FramedWirePart =>
                    if (fpart.material == material) false
                    else
                    {
                        RenderFramedWire.renderCoverHighlight(fpart, material)
                        true
                    }
                case _ => false
            }
        }
    }
}

