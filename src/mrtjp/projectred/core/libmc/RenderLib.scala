package mrtjp.projectred.core.libmc

import net.minecraft.block.Block
import net.minecraft.client.renderer.RenderBlocks
import net.minecraft.world.{World, IBlockAccess}
import java.util.Random
import cpw.mods.fml.client.registry.ISimpleBlockRenderingHandler
import mrtjp.projectred.core.PRLogger

object RenderLib
{
    var multiRenderID = -1

    val renders = Array.ofDim[MultiTileRender](4096, 16)

    def setRenderer(b:Block, meta:Int, r:MultiTileRender)
    {
        val id = Block.getIdFromBlock(b)
        renders(id)(meta) = r
    }

    def getRenderer(b:Block, meta:Int) = renders(Block.getIdFromBlock(b))(meta)
}

object MultiRenderHandler extends ISimpleBlockRenderingHandler
{
    override def getRenderId = RenderLib.multiRenderID

    override def shouldRender3DInInventory(modelId:Int) = true

    override def renderInventoryBlock(b:Block, meta:Int, renderID:Int, r:RenderBlocks)
    {
        if (renderID != RenderLib.multiRenderID) return

        val render = RenderLib.getRenderer(b, meta)
        if (render == null)
        {
            PRLogger.severe("No render mapping found for "+b.getUnlocalizedName+":"+meta)
            return
        }

        render.renderInvBlock(r, meta)
    }

    override def renderWorldBlock(w:IBlockAccess, x:Int, y:Int, z:Int, b:Block, renderID:Int, r:RenderBlocks):Boolean =
    {
        if (r.overrideBlockTexture != null) return true
        if (renderID != RenderLib.multiRenderID) return false

        val meta = w.getBlockMetadata(x, y, z)
        val render = RenderLib.getRenderer(b, meta)

        if (render == null)
        {
            PRLogger.severe("No render mapping found for "+b.getUnlocalizedName+":"+meta)
            return true
        }

        render.renderWorldBlock(r, w, x, y, z, meta)
        true
    }
}

abstract class MultiTileRender(val block:Block)
{
    def renderWorldBlock(r:RenderBlocks, w:IBlockAccess, x:Int, y:Int, z:Int, meta:Int)

    def renderInvBlock(r:RenderBlocks, meta:Int)

    def randomDisplayTick(w:World, x:Int, y:Int, z:Int, r:Random)
}

