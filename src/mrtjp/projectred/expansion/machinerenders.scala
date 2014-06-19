package mrtjp.projectred.expansion

import codechicken.lib.lighting.LightMatrix
import codechicken.lib.render._
import codechicken.lib.vec._
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.expansion.BlockMachine._
import net.minecraft.block.Block
import net.minecraft.client.renderer.RenderBlocks
import net.minecraft.util.IIcon
import net.minecraft.world.IBlockAccess
import mrtjp.projectred.core.libmc.{PRLib, MultiTileRender, PRColors}
import codechicken.lib.render.uv.MultiIconTransformation
import net.minecraft.client.renderer.texture.IIconRegister

@Deprecated
abstract class TileMachineRender(b:Block) extends MultiTileRender(b)
{
    val matrix = new LightMatrix
    val sides =
    {
        val m = CCModel.quadModel(6*4).generateBlock(0, Cuboid6.full.copy().expand(0.0001D))
        m.computeNormals()
        m.shrinkUVs(0.0005D)
        m
    }
    val model =
    {
        val m = CCModel.quadModel(6*4).generateBlock(0, Cuboid6.full)
        m.computeNormals()
        m.shrinkUVs(0.0005D)
        m
    }

    def renderWorldBlock(r:RenderBlocks, w:IBlockAccess, x:Int, y:Int, z:Int, meta:Int)
    {
        matrix.locate(w, x, y, z)
        val tile = PRLib.getTileEntity(w, new BlockCoord(x,y,z), classOf[TileMachineWorking])
        if (tile != null)
        {
            CCRenderState.reset()
            //CCRenderState.useModelColours(true)
            for (s <- 0 until 6)
                if (s != Rotation.rotateSide(tile.side, tile.rotation))
                {
                    sides.setColour(PRColors.get(tile.sideConfig(s)).rgba)
                    sides.render(s*4, 4, Vector3.fromTileEntity(tile).translation(), new uv.IconTransformation(BlockMachine.iconIO), matrix)
                }
                else
                {
                    sides.setColour(-1)
                    val w = new uv.IconTransformation(if (tile.isWorking) BlockMachine.work else BlockMachine.nowork)
                    sides.render(s*4, 4, Vector3.fromTileEntity(tile).translation(), w, matrix)
                }

            val iconT =
            {
                if (r.overrideBlockTexture != null) new uv.IconTransformation(r.overrideBlockTexture)
                else new MultiIconTransformation(bottom, top, side, getFront(tile.isWorking), side, side)
            }

            CCRenderState.reset()
            model.render(tile.rotationT.`with`(Vector3.fromTileEntity(tile).translation()), iconT, matrix)
        }
    }

    def renderForSideSelect(r:Int)
    {
        val iconT = new MultiIconTransformation(bottom, top, side, getFront(false), side, side)
        val s2 = model.copy().apply(Rotation.sideOrientation(0, r).at(Vector3.center).`with`(new Translation(-0.5D, -0.5D, -0.5D)))
        s2.render(null, iconT, null)
    }

    def getFront(working:Boolean):IIcon

    def renderInvBlock(r:RenderBlocks, meta:Int)
    {
        val iconT = new MultiIconTransformation(bottom, top, side, getFront(false), side, side)

        CCRenderState.reset()
        //CCRenderState.useNormals(true)
        //CCRenderState.useModelColours(true)
        CCRenderState.startDrawing()
        model.render(new Translation(-0.5D, -0.5D, -0.5D), iconT)
        CCRenderState.draw()
        CCRenderState.setColour(-1)
    }
}

@Deprecated
object RenderFurnace extends TileMachineRender(ProjectRedExpansion.machine1)
{
    override def getFront(working:Boolean) =
        if (working) BlockMachine.furnaceFrontOn else BlockMachine.furnaceFront
}

object RenderController extends MultiTileRender(ProjectRedExpansion.machine1)
{
    override def renderWorldBlock(r:RenderBlocks, w:IBlockAccess, x:Int, y:Int, z:Int, meta:Int)
    {
        val tile = PRLib.getTileEntity(w, new BlockCoord(x,y,z), classOf[TileRouterController])
        if (tile != null)
        {
        }
    }

    override def renderInvBlock(r:RenderBlocks, meta:Int)
    {
    }

    override def registerIcons(reg:IIconRegister)
    {
    }
}