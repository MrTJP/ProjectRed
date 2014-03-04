package mrtjp.projectred.core.world

import codechicken.lib.vec.Vector3
import net.minecraft.block.Block
import mrtjp.projectred.core.world.Replacement.Replacement

abstract class GeneratorTree(var tree:ITreeGenData) extends WorldGenCore
{
    var startX = 0
    var startY = 0
    var startZ = 0
    var spawnPods = false

    var minPodHeight = 3
    var leaf:BlockType = null
    var wood:BlockType = null
    var vine:BlockType = new BlockType(Block.vine.blockID, 0)
    var air:BlockType = new BlockTypeVoid

    override def sublayer(x:Int, y:Int, z:Int) =
    {
        startX = x
        startY = y
        startZ = z
        spawnPods = tree.hasProducts

        leaf = getLeaf
        wood = getWood

        preGen()
        if (canGrow)
        {
            gen()
            true
        }
        else false
    }

    def getLeaf:BlockType
    def getWood:BlockType

    def preGen()
    def canGrow:Boolean

    def gen()

    def startVec = new Vector3(startX, startY, startZ)

    def genTrunk(height:Int, girth:Int) = genTrunk(height, girth, 0)
    def genTrunk(height:Int, girth:Int, vines:Float)
    {
        val offset = (girth-1)/2
        for (x <- 0 until girth) for (y <- 0 until height) for (z <- 0 until girth)
        {
            addWood(x-offset, y, z-offset, Replacement.All)
            if (rand.nextFloat() < vines) addVine(x-offset-1, y, z-offset)
            if (rand.nextFloat() < vines) addVine(x-offset+1, y, z-offset)
            if (rand.nextFloat() < vines) addVine(x-offset, y, z-offset-1)
            if (rand.nextFloat() < vines) addVine(x-offset, y, z-offset+1)
        }
        if (spawnPods) for (y <- minPodHeight until height) for (x <- 0 until girth) for (z <- 0 until girth)
            if (x <= 0 || x >= girth || z <= 0 || z >= girth)
            {
                tree.tryGrowProducts(world, startX+x+1, startY+y, startZ+z)
                tree.tryGrowProducts(world, startX+x-1, startY+y, startZ+z)
                tree.tryGrowProducts(world, startX+x, startY+y, startZ+z+1)
                tree.tryGrowProducts(world, startX+x, startY+y, startZ+z-1)
            }
    }

    def genStems(height:Int, girth:Int, chance:Float, maxHeight:Float)
    {
        val offset = 1
        val offset2 = girth+offset
        for (x <- -offset until offset2) for (z <- -offset until offset2)
        {
            var failed = false
            if (x == -offset && z == -offset) failed = true
            if (x == offset2 && z == offset2) failed = true
            if (x == -offset && z == offset2) failed = true
            if (x == offset2 && z == -offset) failed = true

            if (!failed)
            {
                val sH = rand.nextInt(Math.round(height*maxHeight))
                if (rand.nextFloat() < chance) for (y <- 0 until sH)
                    addWood(x, y, z, Replacement.Soft)
            }
        }
    }


    override def addBlock(x:Int, y:Int, z:Int, block:BlockType, repl:Replacement)
    {
        if (repl == Replacement.All ||
            (repl == Replacement.Soft && GenLib.isSoft(world, startX+x, startY+y, startZ+z)) ||
            world.isAirBlock(startX+x, startY+y, startZ+z))
                block.setBlock(world, tree, startX+x, startY+y, startZ+z)

    }

    def addWood(x:Int, y:Int, z:Int, repl:Replacement) = addBlock(x, y, z, wood, repl)
    def addLeaf(x:Int, y:Int, z:Int, repl:Replacement) = addBlock(x, y, z, leaf, repl)
    def addVine(x:Int, y:Int, z:Int) = addBlock(x, y, z, vine, Replacement.None)

    def clearBlock (x:Int, y:Int, z:Int) = air.setBlock(world, tree, startX+x, startY+y, startZ+z)
}
