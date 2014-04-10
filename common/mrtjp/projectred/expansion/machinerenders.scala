package mrtjp.projectred.expansion

import codechicken.lib.lighting.LightMatrix
import codechicken.lib.render._
import codechicken.lib.vec._
import java.util.Random
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.core.blockutil.RenderMulti
import mrtjp.projectred.core.{PRColors, BasicUtils}
import mrtjp.projectred.expansion.BlockMachine._
import net.minecraft.block.Block
import net.minecraft.client.renderer.RenderBlocks
import net.minecraft.util.Icon
import net.minecraft.world.{IBlockAccess, World}

abstract class TileMachineRender(b:Block) extends RenderMulti(b)
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
        matrix.computeAt(w, x, y, z)
        val tile = BasicUtils.getTileEntity(w, new BlockCoord(x,y,z), classOf[TileMachineWorking])
        if (tile != null)
        {
            CCRenderState.reset()
            CCRenderState.useModelColours(true)
            for (s <- 0 until 6)
                if (s != Rotation.rotateSide(tile.side, tile.rotation))
                {
                    sides.setColour(PRColors.get(tile.sideConfig(s)).rgba)
                    sides.render(s*4, 4, Vector3.fromTileEntity(tile).translation(), new IconTransformation(BlockMachine.iconIO), matrix)
                }
                else
                {
                    sides.setColour(-1)
                    val w = new IconTransformation(if (tile.isWorking) BlockMachine.work else BlockMachine.nowork)
                    sides.render(s*4, 4, Vector3.fromTileEntity(tile).translation(), w, matrix)
                }

            val iconT =
            {
                if (r.overrideBlockTexture != null) new IconTransformation(r.overrideBlockTexture)
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

    def getFront(working:Boolean):Icon

    def renderInvBlock(r:RenderBlocks, meta:Int)
    {
        val iconT = new MultiIconTransformation(bottom, top, side, getFront(false), side, side)

        CCRenderState.reset()
        CCRenderState.useNormals(true)
        CCRenderState.useModelColours(true)
        CCRenderState.startDrawing(7)
        model.render(new Translation(-0.5D, -0.5D, -0.5D), iconT, null)
        CCRenderState.draw()
        CCRenderState.setColour(-1)
    }

    def randomDisplayTick(w:World, x:Int, y:Int, z:Int, r:Random) {}
}

object RenderFurnace extends TileMachineRender(ProjectRedExpansion.machine1)
{
    override def getFront(working:Boolean) =
        if (working) BlockMachine.furnaceFrontOn else BlockMachine.furnaceFront
}

object RenderController extends RenderMulti(ProjectRedExpansion.machine1)
{
    val model =
    {
        val m = CCModel.quadModel(6*4).generateBlock(0, Cuboid6.full)
        m.computeNormals()
        m.shrinkUVs(0.0005D)
        m
    }
    val matrix = new LightMatrix

    override def renderWorldBlock(r:RenderBlocks, w:IBlockAccess, x:Int, y:Int, z:Int, meta:Int)
    {
        val tile = BasicUtils.getTileEntity(w, new BlockCoord(x,y,z), classOf[TileRouterController])
        if (tile != null)
        {
            matrix.computeAt(w, x, y, z)

            val iconT = if (r.overrideBlockTexture != null) new IconTransformation(r.overrideBlockTexture)
                else new MultiIconTransformation(bottom, top, side, side, side, side)

            model.render(tile.rotationT.`with`(Vector3.fromTileEntity(tile).translation()), iconT, matrix)
        }
    }

    override def renderInvBlock(r:RenderBlocks, meta:Int) {}

    override def randomDisplayTick(w:World, x:Int, y:Int, z:Int, r:Random) {}
}