/*
/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.world

import java.util.Random

import mrtjp.core.math.MathLib
import net.minecraft.block.Block
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World

class WorldGenDecorator extends TWorldGenerator
{
    var cluster = Set[((Block, Int), Int)]()
    var material = Set[(Block, Int)]()
    var soil = Set[(Block, Int)]()
    var clusterSize = 1

    var checkStay = true
    var seeSky = false
    var underTree = false

    var stackHeight = 1
    var dx = 4
    var dy = 2
    var dz = 4

    override def generate(w:World, rand:Random, pos:BlockPos) =
    {
        val xStart = pos.getX
        val yStart = pos.getY
        val zStart = pos.getZ
        var generated = false
        for (i <- 0 until cluster.size)
        {
            val x = xStart+rand.nextInt(dx)-rand.nextInt(dx)
            var y = yStart+1+(if (dy > 1) rand.nextInt(dy)-rand.nextInt(dy) else 0)
            val z = zStart+rand.nextInt(dz)-rand.nextInt(dz)

            if (checkLocation(w, x, y, z))
            {
                val (b, m) = MathLib.weightedRandom(cluster)
                val state = b.getStateFromMeta(m)
                val h = if (stackHeight > 1) rand.nextInt(stackHeight) else 0

                import scala.util.control.Breaks._
                breakable(for (s <- 0 to h)
                {
                    if (!checkStay) generated |= w.setBlockState(new BlockPos(x, y, z), state, 2)
                    else break()
                    y += 1
                    if (!canSetBlock(w, new BlockPos(x, y, z), material)) break()
                })
            }
        }
        generated
    }

    def checkLocation(w:World, x:Int, y:Int, z:Int):Boolean =
    {
        val pos = new BlockPos(x,y,z)
        if (seeSky && !w.canSeeSky(pos)) return false
        if (!canSetBlock(w, pos, material)) return false
        if (!canSetBlock(w, pos.down(), soil)) return false
        true
    }
}
*/
