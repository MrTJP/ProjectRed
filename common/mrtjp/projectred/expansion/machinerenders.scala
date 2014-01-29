package mrtjp.projectred.expansion

import java.util.Random
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.core.blockutil.RenderMulti
import net.minecraft.block.Block
import net.minecraft.client.renderer.RenderBlocks
import net.minecraft.world.{IBlockAccess, World}

class TileMachineCommonRender(b:Block) extends RenderMulti(b)
{
    def renderWorldBlock(r:RenderBlocks, w:IBlockAccess, x:Int, y:Int, z:Int, meta:Int) {}

    def renderInvBlock(r:RenderBlocks, meta:Int) {}

    def randomDisplayTick(w:World, x:Int, y:Int, z:Int, r:Random) {}
}

object TileElectricFurnaceRenderer extends RenderMulti(ProjectRedExpansion.machine1)
{
    override def renderWorldBlock(r:RenderBlocks, w:IBlockAccess, x:Int, y:Int, z:Int, meta:Int)
    {

    }

    def renderInvBlock(r:RenderBlocks, meta:Int)
    {
    }

    def randomDisplayTick(w:World, x:Int, y:Int, z:Int, r:Random) {}
}

