package mrtjp.projectred.core.libmc.world

import net.minecraft.world.World
import net.minecraft.init.Blocks

object GenLib
{
    def isSoft(world:World, x:Int, y:Int, z:Int) =
    {
        val b = world.getBlock(x, y, z)
        (b == Blocks.vine) ||
            (b == Blocks.tallgrass) ||
            (b == Blocks.deadbush) ||
            (b == Blocks.snow) ||
            b.isReplaceable(world, x, y, z)
    }
}
