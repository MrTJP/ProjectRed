/*
/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.world

import java.util.Random

import net.minecraft.block.Block
import net.minecraft.util.EnumFacing
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World
import net.minecraftforge.fml.common.FMLLog

import scala.annotation.tailrec
import scala.collection.immutable.Queue

class WorldGenCaveReformer extends TWorldGenerator {
    var cluster = Set[((Block, Int), Int)]()
    var material = Set[(Block, Int)]()
    var clusterSize = 1
    var depth = 5
    var searchRadius = 16

    override def generate(w: World, rand: Random, pos: BlockPos): Boolean = {
        if (!w.isAirBlock(pos)) return false
        var dy = 0
        while (dy < searchRadius && !canSetBlock(w, pos.add(0, dy, 0), material)) dy += 1
        if (!canSetBlock(w, pos.add(0, dy, 0), material)) return false

        val start = Node(pos.add(0, dy, 0))
        start.depth = depth
        iterate(w, Queue(start))
    }

    @tailrec
    private def iterate(w: World, open: Seq[Node], closed: Set[Node] = Set.empty, generated: Boolean = false): Boolean = open match {
        case Seq() => generated

        case Seq(next, rest@_*) =>
            if (closed.size > clusterSize) return generated
            val g2 = setBlock(w, next.bc, cluster, material)
            val upNext = Vector.newBuilder[Node]
            if (next.depth > 0)
                for (s <- 0 until 6) {
                    val to = next --> EnumFacing.values()(s)
                    if (!open.contains(to) && !closed.contains(to)) {
                        to.depth = if (WorldLib.isBlockTouchingAir(w, to.bc)) depth else next.depth - 1
                        upNext += to
                    }
                }
            iterate(w, rest ++ upNext.result(), closed + next, generated || g2)

        case _ => generated
    }

    private object Node {
        def apply(bp: BlockPos): Node = new Node(bp, 0)

        def apply(bp: BlockPos, dir: EnumFacing): Node = new Node(bp.offset(dir), 1)

        @Deprecated
        def apply(bp: BlockPos, dir: Int): Node = new Node(bp.offset(EnumFacing.values()(dir)), 1)
    }

    private class Node(val bc: BlockPos, val dist: Int) extends Ordered[Node] {
        var depth = 0

        def -->(toDir: EnumFacing, distAway: Int): Node = new Node(bc.offset(toDir), dist + distAway)
        def -->(toDir: EnumFacing): Node = this --> (toDir, 1)

        override def compare(that: Node) = dist - that.dist

        override def equals(other: Any) = other match {
            case that: Node => bc == that.bc
            case _ => false
        }

        override def hashCode = bc.hashCode

        override def toString = "@" + bc.toString
    }

}
*/
