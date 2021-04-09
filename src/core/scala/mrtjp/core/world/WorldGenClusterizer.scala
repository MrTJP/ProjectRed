/*
/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.world

import java.util.Random

import codechicken.lib.math.MathHelper
import net.minecraft.block.Block
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World

class WorldGenClusterizer extends TWorldGenerator
{
    var cluster = Set[((Block, Int), Int)]()
    var material = Set[(Block, Int)]()
    var clusterSize = 1

    override def generate(w:World, rand:Random, pos:BlockPos) =
        if (clusterSize < 4) generateSmall(w, rand, pos) else generateNormal(w, rand, pos)

    def generateSmall(w:World, rand:Random, pos:BlockPos):Boolean =
    {
        var generated = false
        for (i <- 0 until clusterSize)
        {
            val p = pos.add(rand.nextInt(2),rand.nextInt(2),rand.nextInt(2))
            generated |= setBlock(w, p, cluster, material)
        }
        generated
    }

    def generateNormal(w:World, rand:Random, pos:BlockPos):Boolean =
    {
        val f = rand.nextFloat*Math.PI.toFloat
        val xNDir = pos.getX+8+(MathHelper.sin(f)*clusterSize)/8F
        val xPDir = pos.getX+8-(MathHelper.sin(f)*clusterSize)/8F
        val zNDir = pos.getZ+8+(MathHelper.cos(f)*clusterSize)/8F
        val zPDir = pos.getZ+8-(MathHelper.cos(f)*clusterSize)/8F
        val yNDir = (pos.getY+rand.nextInt(3))-2
        val yPDir = (pos.getY+rand.nextInt(3))-2

        val dx = xPDir-xNDir
        val dy = yPDir-yNDir
        val dz = zPDir-zNDir

        var generated = false
        for (i <- 0 until clusterSize)
        {
            val xCenter = xNDir+(dx*i)/clusterSize
            val yCenter = yNDir+(dy*i)/clusterSize
            val zCenter = zNDir+(dz*i)/clusterSize

            val size = (rand.nextDouble.toFloat*clusterSize)/16f

            val hMod = ((MathHelper.sin((i*Math.PI.toFloat)/clusterSize)+1f)*size+1f)*0.5f
            val vMod = ((MathHelper.sin((i*Math.PI.toFloat)/clusterSize)+1f)*size+1f)*0.5f

            val x0 = MathHelper.floor(xCenter-hMod)
            val y0 = MathHelper.floor(yCenter-vMod)
            val z0 = MathHelper.floor(zCenter-hMod)

            val x1 = MathHelper.floor(xCenter+hMod)
            val y1 = MathHelper.floor(yCenter+vMod)
            val z1 = MathHelper.floor(zCenter+hMod)

            for (blockX <- x0 to x1)
            {
                var xDistSq = ((blockX+0.5f)-xCenter)/hMod
                xDistSq *= xDistSq
                if (xDistSq < 1f) for (blockY <- y0 to y1)
                {
                    var yDistSq = ((blockY+0.5f)-yCenter)/vMod
                    yDistSq *= yDistSq
                    val xyDistSq = yDistSq+xDistSq
                    if (xyDistSq < 1f) for (blockZ <- z0 to z1)
                    {
                        var zDistSq = ((blockZ+0.5f)-zCenter)/hMod
                        zDistSq *= zDistSq
                        if (zDistSq+xyDistSq < 1f)
                            generated |= setBlock(w, new BlockPos(blockX, blockY, blockZ), cluster, material)
                    }
                }
            }
        }
        generated
    }
}
*/
