package mrtjp.projectred.transportation

import codechicken.lib.render.{CCRenderState, TextureUtils}
import codechicken.lib.vec.{Translation, Scale, BlockCoord, Vector3}
import codechicken.multipart.{TItemMultiPart, MultiPartRegistry}
import cpw.mods.fml.relauncher.{SideOnly, Side}
import java.util.{List => JList}
import mrtjp.projectred.ProjectRedTransportation
import net.minecraft.block.Block
import net.minecraft.client.renderer.texture.IconRegister
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.world.World
import net.minecraftforge.client.IItemRenderer
import net.minecraftforge.client.IItemRenderer.{ItemRendererHelper, ItemRenderType}

class ItemPartPipe(id:Int) extends Item(id) with TItemMultiPart
{
    setHasSubtypes(true)
    setCreativeTab(ProjectRedTransportation.tabTransportation)
    setUnlocalizedName("projectred.transportation.pipe")

    def newPart(item:ItemStack, player:EntityPlayer, world:World, pos:BlockCoord, side:Int, vhit:Vector3) =
    {
        val pdef = PipeDef.VALID_PIPE(item.getItemDamage)
        val p = MultiPartRegistry.createPart(pdef.partname, false).asInstanceOf[FlowingPipePart]
        if (p != null) p.preparePlacement(side, item.getItemDamage)
        p
    }

    override def onItemUse(stack:ItemStack, player:EntityPlayer, w:World, x:Int, y:Int, z:Int, side:Int, hitX:Float, hitY:Float, hitZ:Float) =
    {
        if (super.onItemUse(stack, player, w, x, y, z, side, hitX, hitY, hitZ))
        {
            w.playSoundEffect(x+0.5, y+0.5, z+0.5, Block.soundGlassFootstep.getPlaceSound,
                Block.soundGlassFootstep.getVolume*5.0F, Block.soundGlassFootstep.getPitch*0.9F)
            true
        }
        else false
    }

    override def getUnlocalizedName(stack:ItemStack) = super.getUnlocalizedName+"|"+stack.getItemDamage

    @SideOnly(Side.CLIENT)
    override def getSubItems(id:Int, tab:CreativeTabs, list:JList[_])
    {
        val l2 = list.asInstanceOf[JList[ItemStack]]
        for (t <- PipeDef.VALID_PIPE) l2.add(t.getItemStack)
    }

    override def registerIcons(reg:IconRegister)
    {
        for (p <- PipeDef.VALID_PIPE) p.loadTextures(reg)
    }

    @SideOnly(Side.CLIENT)
    override def getSpriteNumber = 0
}

object PipeItemRenderer extends IItemRenderer
{
    def handleRenderType(item:ItemStack, r:ItemRenderType) = true
    def shouldUseRenderHelper(r:ItemRenderType, item:ItemStack, helper:ItemRendererHelper) = true

    def renderItem(rtype:ItemRenderType, item:ItemStack, data:AnyRef*)
    {
        val damage = item.getItemDamage
        import ItemRenderType._
        rtype match
        {
            case ENTITY => renderWireInventory(damage, -.5f, 0f, -.5f, 1f)
            case EQUIPPED => renderWireInventory(damage, 0f, .0f, 0f, 1f)
            case EQUIPPED_FIRST_PERSON => renderWireInventory(damage, 1f, -.6f, -.4f, 2f)
            case INVENTORY => renderWireInventory(damage, 0f, -.1f, 0f, 1f)
            case _ =>
        }
    }

    def renderWireInventory(meta:Int, x:Float, y:Float, z:Float, scale:Float)
    {
        val pdef:PipeDef = PipeDef.VALID_PIPE(meta)
        if (pdef == null) return
        TextureUtils.bindAtlas(0)
        CCRenderState.reset()
        CCRenderState.useNormals(true)
        CCRenderState.pullLightmap()
        CCRenderState.setColourOpaque(-1)
        CCRenderState.startDrawing(7)

        RenderPipe.renderInv(new Scale(scale).`with`(new Translation(x, y, z)), pdef.sprites(0))

        CCRenderState.draw()
    }
}

