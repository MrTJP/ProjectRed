package mrtjp.projectred.expansion

import codechicken.lib.render.uv.MultiIconTransformation
import codechicken.lib.render.{BlockRenderer, CCRenderState, TextureUtils}
import codechicken.lib.vec.{Cuboid6, Translation}
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.core.libmc.{MultiTileRender, PRLib}
import net.minecraft.client.renderer.RenderBlocks
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.util.IIcon
import net.minecraft.world.IBlockAccess

object RenderFurnace extends MultiTileRender(ProjectRedExpansion.machine1)
{
    override def renderWorldBlock(r:RenderBlocks, w:IBlockAccess, x:Int, y:Int, z:Int, meta:Int){}

    override def renderInvBlock(r:RenderBlocks, meta:Int){}

    override def registerIcons(reg:IIconRegister){}

    override def getIcon(side:Int, meta:Int) = null
}

object RenderController extends MultiTileRender(ProjectRedExpansion.machine2)
{
    var side:IIcon = _
    var topOn:IIcon = _
    var topOff:IIcon = _
    var bottom:IIcon = _

    var iconsOn:MultiIconTransformation = _
    var iconsOff:MultiIconTransformation = _

    override def renderWorldBlock(r:RenderBlocks, w:IBlockAccess, x:Int, y:Int, z:Int, meta:Int)
    {
        val tile = PRLib.getTileEntity(w, x, y, z, classOf[TileRouterController])
        if (tile != null)
        {
            TextureUtils.bindAtlas(0)
            CCRenderState.reset()
            CCRenderState.lightMatrix.locate(w, x, y, z)
            CCRenderState.setPipeline(CCRenderState.lightMatrix, new Translation(x, y, z), if (tile.operational) iconsOn else iconsOff)
            BlockRenderer.renderCuboid(Cuboid6.full, 0)
        }
    }

    private val invTranslation = new Translation(0, -0.075, 0)
    override def renderInvBlock(r:RenderBlocks, meta:Int)
    {
        TextureUtils.bindAtlas(0)
        CCRenderState.reset()
        CCRenderState.setDynamic()
        CCRenderState.pullLightmap()
        CCRenderState.setPipeline(invTranslation, iconsOff)
        CCRenderState.startDrawing()
        BlockRenderer.renderCuboid(Cuboid6.full, 0)
        CCRenderState.draw()
    }

    override def registerIcons(reg:IIconRegister)
    {
        side = reg.registerIcon("projectred:machines/routercontroller/side")
        topOn = reg.registerIcon("projectred:machines/routercontroller/topon")
        topOff = reg.registerIcon("projectred:machines/routercontroller/topoff")
        bottom = reg.registerIcon("projectred:machines/routercontroller/bottom")

        iconsOn = new MultiIconTransformation(bottom, topOn, side, side, side, side)
        iconsOff = new MultiIconTransformation(bottom, topOff, side, side, side, side)
    }

    override def getIcon(s:Int, meta:Int) = s match
    {
        case 0 => bottom
        case 1 => topOff
        case _ => side
    }
}