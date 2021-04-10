/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.relocation

import java.util.{Set => JSet}

import mrtjp.projectred.api._
import net.minecraft.block.Block
import net.minecraft.block.properties.IProperty
import net.minecraft.block.state.IBlockState
import net.minecraft.util.ResourceLocation
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World

import scala.collection.JavaConversions._
import scala.util.matching.Regex

object StickRegistry
{
    val rKeyVal:Regex = raw"([\w:#=,]+)\s*->\s*(.+)".r

    var latchMap:Map[BlockStateFilter, Set[BlockStateFilter]] = Map().withDefaultValue(Set())
    var latchOps:List[(BlockStateFilter, BlockStateFilter) => Boolean] = List() // FIXME this doesn't seem to be used?
    var interactionList:Seq[IFrameInteraction] = Seq()

    def parseKV(kv:Seq[String]):Seq[(String, String)] = kv.map {
        case rKeyVal(k, v) => (k, v)
        case s => throw new MatchError(s"Illegal [k -> v] pair: $s")
    }

    def parseAndAddLatchSets(kv:Seq[String]):Array[String] =
    {
        parseKV(kv).foreach(b => addLatchSet(BlockStateFilter.fromString(b._1), BlockStateFilter.fromString(b._2)))
        latchMap.flatMap { kv =>
            val e1 = kv._1.toString
            kv._2.map { k =>
                val e2 = k.toString
                e1 + " -> " + e2
            }
        }.toArray
    }

    def addLatchSet(b1:BlockStateFilter, b2:BlockStateFilter)
    {
        latchMap += b1 -> (latchMap(b1) + b2)
    }

    def areBlocksLatched(w:World, pos1:BlockPos, pos2:BlockPos) =
    {
        val b1 = w.getBlockState(pos1)
        val b2 = w.getBlockState(pos2)
        latchMap.exists(it => it._1.matches(b1) && it._2.exists(_.matches(b2)))
    }

    def getFrame(w:World, pos:BlockPos):IFrame = {
        val b = w.getBlockState(pos).getBlock
        if (b.isInstanceOf[IFrame]) return b.asInstanceOf[IFrame]

        val te = w.getTileEntity(pos)
        if (te != null && te.isInstanceOf[IFrame])
            return te.asInstanceOf[IFrame]

        if (te != null && te.hasCapability(IRelocationAPI.FRAME_CAPABILITY, null))
            return te.getCapability(IRelocationAPI.FRAME_CAPABILITY, null)

        interactionList.find(_.canInteract(w, pos)).orNull
    }
}

case class BlockStateFilter(block:Block, constraints:Map[IProperty[_], Comparable[_]])
{
    private def getPropValue[T <: Comparable[T]](prop:IProperty[T], value:Comparable[_]):String = prop.getName(value.asInstanceOf[T])

    def matches(state:IBlockState):Boolean =
        state.getBlock == block &&
                constraints.forall(pv => state.getValue(pv._1) == pv._2)

    override val toString:String = {
        var s = block.getRegistryName.toString
        if (constraints.nonEmpty) {
            s += "#" + constraints
                    .map(it => it._1.getName + "=" + getPropValue(it._1, it._2))
                    .fold("")((a, b) => if (a.nonEmpty) s"$a,$b" else b)
        }
        s
    }
}

object BlockStateFilter
{
    def fromString(expr:String):BlockStateFilter = expr.split("#") match {
        case Array(blockRL) => BlockStateFilter(Option(Block.REGISTRY.getObject(new ResourceLocation(blockRL))).get, Map())
        case Array(blockRL, constraintsSpec) =>
            val block = Option(Block.REGISTRY.getObject(new ResourceLocation(blockRL))).get
            val constraints = constraintsSpec.split(",")
                    .map(it => it.split("=") match {
                        case Array(key, value) =>
                            val prop = block.getBlockState.getProperty(key)
                            val pvalue = prop.parseValue(value).get
                            (prop, pvalue)
                        case _ => throw new MatchError(s"Illegal key/value pair $it")
                    })
            BlockStateFilter(block, constraints.toMap)
        case _ => throw new MatchError(s"Illegal blockstate spec: $expr")
    }

    def fromBlockState(state:IBlockState):BlockStateFilter =
        BlockStateFilter(state.getBlock, state.getProperties.toMap)
}