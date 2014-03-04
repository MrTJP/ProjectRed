package mrtjp.projectred.core.world

import net.minecraft.block.Block
import net.minecraft.world.World

object GenLib
{
    def isSoft(world:World, x:Int, y:Int, z:Int) =
    {
        val id = world.getBlockId(x, y, z)
        (id == Block.vine.blockID) ||
            (id == Block.tallGrass.blockID) ||
            (id == Block.deadBush.blockID) ||
            (id == Block.snow.blockID) ||
            ((Block.blocksList(id) != null) && Block.blocksList(id).isBlockReplaceable(world, x, y, z))
    }
}
