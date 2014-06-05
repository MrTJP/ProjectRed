package mrtjp.projectred.core.libmc.world

import net.minecraft.block.{BlockAir, Block}
import mrtjp.projectred.core.libmc.world.GeneratorOre.Evaluation
import net.minecraft.world.World
import java.util.Random
import codechicken.lib.vec.BlockCoord

class GeneratorCave(b:Block, meta:Int, vein:Int) extends GeneratorOre(b, meta, vein)
{
    var openList = Vector[Evaluation]()
    var closedList = Set[Evaluation]()

    val MAX_DIAMETER = 128

    def openListPoll =
    {
        val out = openList(0)
        openList = openList.filterNot(_ == out)
        out
    }

    override def generate(world:World, random:Random, x:Int, y:Int, z:Int):Boolean =
    {
        if (!world.getBlock(x, y, z).isInstanceOf[BlockAir]) return false

        var yIndex:Int = y

        while (yIndex <= MAX_DIAMETER && world.getBlock(x, yIndex, z) != Block.getBlockFromName("stone")) yIndex+=1

        addBlockForEvaluation(x, yIndex, z, 6)

        while (openList.size > 0 && vein > 0)
        {
            val eval = openListPoll
            checkStoneBlock(world, eval.x, eval.y, eval.z, eval.sides)
        }
        true
    }

    private def checkStoneBlock(world:World, x:Int, y:Int, z:Int, sides:Int)
    {
        if (world.getBlock(x, y, z) == Block.getBlockFromName("stone"))
        {
            world.setBlock(x, y, z, b, meta, 2)
            if (sides > 0) evaluateNeighbors(world, x, y, z, sides - 1)
            veinSize -= 1
        }
    }

    private def isBlockTouchingAir(w:World, b:BlockCoord):Boolean =
    {
        for (s <- 0 until 6)
        {
            val bc = b.copy().offset(s)
            if (w.getBlock(bc.x, bc.y, bc.z) == Block.getBlockFromName("air")) return true
        }
        false
    }

    private def evaluateNeighbors(w:World, x:Int, y:Int, z:Int, sides:Int)
    {
        val b = new BlockCoord(x, y, z)
        val air = isBlockTouchingAir(w, b)
        for (s <- 0 until 6)
        {
            val bc = b.copy().offset(s)
            addBlockForEvaluation(bc.x, bc.y, bc.z, if (air) 6 else sides)
        }
    }

    private def addBlockForEvaluation(x:Int, y:Int, z:Int, sides:Int)
    {
        val eval = new Evaluation(x, y, z, sides)
        if (closedList.contains(eval)) return
        openList :+= eval
        closedList += eval
    }
}
