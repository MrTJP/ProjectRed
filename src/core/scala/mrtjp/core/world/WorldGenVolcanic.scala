/*
/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.world

import java.util.Random

import codechicken.lib.util.BlockUtils
import mrtjp.core.math.MathLib
import net.minecraft.block.Block
import net.minecraft.init.Blocks
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World

import scala.collection.mutable.{HashMap => MHashMap, Queue => MQueue}

class WorldGenVolcanic extends TWorldGenerator
{
    var ashCluster = Set[((Block, Int), Int)]()
    var conduitCluster = Set[((Block, Int), Int)]()
    var liq:(Block, Int) = null
    var material = Set[(Block, Int)]()
    var materialStart = Set[(Block, Int)]()

    var sizeMin = 32000
    var sizeMax = 64000

    private val stack = MQueue[(Int, Int, Int)]()
    private val test = MHashMap[(Int, Int), Int]()

    override def generate(w:World, rand:Random, pos:BlockPos):Boolean =
    {
        if (!canSetBlock(w, pos, materialStart)) return false
        stack.clear()
        test.clear()

        val x = pos.getX
        val z = pos.getZ

        val swh = WorldLib.findSurfaceHeight(w, pos).getY
        var n = swh

        for (i <- pos.getY until swh)
        {
            val p = new BlockPos(x, i, z)
            setBlock(w, p, liq, material)
            setBlock(w, p.north, conduitCluster, material)
            setBlock(w, p.south, conduitCluster, material)
            setBlock(w, p.east, conduitCluster, material)
            setBlock(w, p.west, conduitCluster, material)
        }

        val head = 3+rand.nextInt(4)
        val spread = rand.nextInt(3)

        var size = MathLib.randomFromIntRange(sizeMin until sizeMax, rand)

        import scala.util.control.Breaks.{break, breakable}
        breakable(while (size > 0)
        {
            while (stack.size == 0)
            {
                setBlock(w, new BlockPos(x, n, z), liq, material)
                test.clear()
                enqueueBlocks(x, n, z, head, rand)
                n += 1
                if (n > 125) break()
            }

            val (i, j, k) = stack.dequeue()
            w.getBlockState(new BlockPos(i, 64, k)) //force chunk generation
            if (w.isBlockLoaded(new BlockPos(i, 64, k)) && test.contains((i, k)))
            {
                var pow = test((i, k))
                var hm = w.getHeight(i, k)+1
                while (hm > 0 && isUnimportant(w, i, hm-1, k)) hm -= 1

                if (hm <= j) if (isUnimportant(w, i, hm, k))
                {
                    purgeArea(w, i, hm, k)
                    setBlock(w, new BlockPos(i, hm, k), ashCluster, material)
                    if (j > hm) pow = math.max(pow, spread)
                    enqueueBlocks(i, hm, k, pow, rand)
                    size -= 1
                }
            }
        })

        setBlock(w, new BlockPos(x, n, z), liq, material)
        var p = new BlockPos(x, n, z)
        while (n >= swh && liq._1 == w.getBlockState(p).getBlock)
        {
            p = new BlockPos(x, n, z)
            BlockUtils.fireBlockUpdate(w, p)
            w.notifyNeighborsRespectDebug(p, liq._1, false)
            w.neighborChanged(p, liq._1, p)
            w.immediateBlockTick(p, w.getBlockState(p), w.rand)
            n -= 1
        }
        true
    }

    private def purgeArea(w:World, x:Int, y:Int, z:Int)
    {
        if (w.isAirBlock(new BlockPos(x, y, z))) return
        for (i <- -1 to 1) for (j <- -1 to 1)
        {
            val p = new BlockPos(x+i, y, z+j)
            val b = w.getBlockState(p)
            if ((b.getBlock == Blocks.SNOW) || WorldLib.isAssociatedTreeBlock(w, p, b))
                w.setBlockToAir(p)
        }
        purgeArea(w, x, y+1, z)
    }

    private def enqueueBlocks(x:Int, y:Int, z:Int, p:Int, rand:Random)
    {
        val seed = rand.nextInt(16)
        enq(x-1, y, z, if ((seed&1) != 0) p-1 else p)
        enq(x+1, y, z, if ((seed&2) != 0) p-1 else p)
        enq(x, y, z-1, if ((seed&4) != 0) p-1 else p)
        enq(x, y, z+1, if ((seed&8) != 0) p-1 else p)

        def enq(x:Int, y:Int, z:Int, p:Int)
        {
            if (p > 0)
            {
                val o = test.getOrElse((x, z), -1)
                if (p > o)
                {
                    stack.enqueue((x, y, z))
                    test += (x, z) -> p
                }
            }
        }
    }

    private def isUnimportant(w:World, x:Int, y:Int, z:Int):Boolean =
    {
        val p = new BlockPos(x,y,z)
        val s = w.getBlockState(p)
        val b = s.getBlock
        if (WorldLib.isBlockSoft(w, p, s) || WorldLib.isAssociatedTreeBlock(w, p, s)) return true
        if (b == Blocks.FLOWING_WATER || b == Blocks.WATER ||b == Blocks.SNOW || b == Blocks.ICE) return true
        false
    }
}
*/
