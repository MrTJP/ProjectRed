package mrtjp.projectred.core.world

import codechicken.lib.vec.Vector3
import java.util.Random
import net.minecraft.world.World
import net.minecraft.world.gen.feature.WorldGenerator
import mrtjp.projectred.core.world.Replacement.Replacement

abstract class WorldGenCore extends WorldGenerator
{
    var world:World = _
    var rand:Random = _

    override def generate(w:World, r:Random, i:Int, j:Int, k:Int) =
    {
        world = w
        rand = r
        val stat = sublayer(i, j, k)
        world = null
        rand = null
        stat
    }

    def sublayer(x:Int, y:Int, z:Int) = false

    def addBlock(x:Int, y:Int, z:Int, blockdef:BlockType, repl:Replacement)

    def genCube(start:Vector3, area:Vector3, block:BlockType, repl:Replacement)
    {
        WorldGenCore.explode(start, area)((x:Int, y:Int, z:Int) => addBlock(x, y, z, block, repl))
    }

    def genCylinder(center:Vector3, radius:Float, height:Int, block:BlockType, repl:Replacement)
    {
        val start = new Vector3(center.x - radius, center.y, center.z - radius)
        val area = new Vector3(radius * 2.0F + 1.0F, height, radius * 2.0F + 1.0F)

        WorldGenCore.explode(start, area)((x:Int, y:Int, z:Int) =>
        {
            if (WorldGenCore.distance(new Vector3(x, y, z), new Vector3(center.x, y, center.z)) <= radius + 0.01D)
                addBlock(x, y, z, block, repl)
        })
    }

    def genCircle(center:Vector3, radius:Float, width:Int, height:Int, block:BlockType, repl:Replacement)
    {
        genCircle(center, radius, width, height, block, 1.0F, repl)
    }

    def genCircle(center:Vector3, radius:Float, width:Int, height:Int, block:BlockType, chance:Float, repl:Replacement)
    {
        val start = new Vector3(center.x - radius, center.y, center.z - radius)
        val area = new Vector3(radius * 2.0F + 1.0F, height, radius * 2.0F + 1.0F)

        WorldGenCore.explode(start, area)((x:Int, y:Int, z:Int) => if (rand.nextFloat <= chance)
        {
            val dist = WorldGenCore.distance(new Vector3(x, y, z), new Vector3(center.x, y, center.z))
            if ((radius - width - 0.01D < dist) && (dist <= radius + 0.01D)) addBlock(x, y, z, block, repl)
        })
    }

    def genSphere(center:Vector3, radius:Int, block:BlockType, repl:Replacement)
    {
        val start = new Vector3(center.x - radius, center.y - radius, center.z - radius)
        val area = new Vector3(radius * 2 + 1, radius * 2 + 1, radius * 2 + 1)

        WorldGenCore.explode(start, area)((x:Int, y:Int, z:Int) =>
        {
            if (WorldGenCore.distance(new Vector3(x, y, z), new Vector3(center.x, center.y, center.z)) <= radius + 0.01D)
                addBlock(x, y, z, block, repl)
        })
    }
}

object WorldGenCore
{
    def explode(a:Vector3, b:Vector3)(p1:(Int, Int, Int) => _)
    {
        for (x <- a.x.asInstanceOf[Int] until (a.x + b.x).asInstanceOf[Int])
            for (y <- a.y.asInstanceOf[Int] until (a.y + b.y).asInstanceOf[Int])
                for (z <- a.z.asInstanceOf[Int] until (a.z + b.z).asInstanceOf[Int])
                    p1(x, y, z)
    }

    def distance(a:Vector3, b:Vector3) = Math.sqrt(Math.pow(a.x - b.x, 2.0D) + Math.pow(a.y - b.y, 2.0D) + Math.pow(a.z - b.z, 2.0D))
}

class BlockType(id:Int, meta:Int)
{
    def setBlock(world:World, tree:ITreeGenData, x:Int, y:Int, z:Int)
    {
        world.setBlock(x, y, z, id, meta, 2)
        if (world.getBlockTileEntity(x, y, z) != null)
            world.removeBlockTileEntity(x, y, z)
    }
}

class BlockTypeVoid extends BlockType(0, 0)

object Replacement extends Enumeration
{
    type Replacement = Value
    val None, All, Soft = Value
}

trait ITreeGenData
{
    def height:Int

    def heightVar:Int

    def girth:Int

    def girthVar:Int

    def canGrow(world:World, x:Int, y:Int, z:Int, withGirth:Int, withHeight:Int):Boolean

    def setLeaves(world:World, x:Int, y:Int, z:Int)

    def setWood(world:World, x:Int, y:Int, z:Int)

    def hasProducts:Boolean

    def tryGrowProducts(world:World, x:Int, y:Int, z:Int):Boolean
}