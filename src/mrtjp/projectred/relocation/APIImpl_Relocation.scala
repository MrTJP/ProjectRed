/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.relocation

import java.util.{Set => JSet}

import mrtjp.projectred.api._
import net.minecraft.util.EnumFacing
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World

import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.collection.immutable.Queue

object APIImpl_Relocation extends IRelocationAPI
{
    var isPreInit = true

    override def registerTileMover(name:String, desc:String, handler:ITileMover) =
    {
        assert(isPreInit)
        MovingTileRegistry.registerTileMover(name, desc, handler)
    }

    override def registerPreferredMover(key:String, value:String)
    {
        assert(isPreInit)
        MovingTileRegistry.preferredMovers :+= (key, value)
    }

    override def registerMandatoryMover(key:String, value:String)
    {
        assert(isPreInit)
        MovingTileRegistry.mandatoryMovers :+= (key, value)
    }

    override def registerFrameInteraction(interaction:IFrameInteraction)
    {
        StickRegistry.interactionList :+= interaction
    }

    override def getRelocator = Relocator_Impl

    override def getStickResolver = StickResolver_Impl

    override def isMoving(world:World, pos:BlockPos) =
        MovementManager.isMoving(world, pos)
}

object StickResolver_Impl extends StickResolver
{
    private var world:World = _
    private var start:BlockPos = _
    private var excl:Set[BlockPos] = _

    override def getStructure(w:World, pos:BlockPos, ex:BlockPos*):JSet[BlockPos] =
    {
        world = w
        start = pos
        excl = ex.toSet
        val result = iterate(Queue(start))
        world = null
        start = null
        excl = null
        result
    }

    @tailrec
    private def iterate(open:Seq[BlockPos], closed:Set[BlockPos] = Set.empty):Set[BlockPos] = open match {
        case Seq() => closed
        case Seq(next, rest@_*) =>
            val toCheck = Vector.newBuilder[BlockPos]
            for (s <- 0 until 6) {
                val side = EnumFacing.getFront(s)
                val to = next.offset(side)
                if (!closed(to) && !open.contains(to) && !excl(to))
                    if (StickRegistry.resolveStick(world, next, side)) {
                        if (!world.isAirBlock(to) && !ProjectRedAPI.relocationAPI.isMoving(world, to))
                            toCheck += to
                    }
            }
            iterate(rest ++ toCheck.result(), closed + next)
    }
}