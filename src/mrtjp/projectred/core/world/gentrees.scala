package mrtjp.projectred.core.world

import codechicken.lib.vec.Vector3
import mrtjp.projectred.core.world.Replacement.Replacement
import net.minecraft.util.MathHelper
import net.minecraft.world.World
import net.minecraft.block.Block

abstract class GeneratorCustomTreeBase(tree:ITreeGenData) extends GeneratorTreeBase(tree)
{
    var girth = 0
    var height = 0

    def getCenteredAt(yCenter:Int, offset:Int) =
    {
        val cent = if (girth%2 == 0) 0.5F else 0.0F
        new Vector3(cent + offset, yCenter, cent + offset)
    }

    def gen()
    {
        genTrunk(height, girth)
        var leafSpawn = height+1

        genAdjustedCylinder(leafSpawn, 0.0F, 1, leaf, Replacement.None)
        leafSpawn -= 1
        genAdjustedCylinder(leafSpawn, 0.5F, 1, leaf, Replacement.None)
        leafSpawn -= 1
        genAdjustedCylinder(leafSpawn, 1.9F, 1, leaf, Replacement.None)
        leafSpawn -= 1
        genAdjustedCylinder(leafSpawn, 1.9F, 1, leaf, Replacement.None)
        leafSpawn -= 1
    }

    def genAdjustedCylinder(yCenter:Int, radius:Float, height:Int, block:BlockType)
    {
        genAdjustedCylinder(yCenter, 0, radius, height, block, Replacement.None)
    }

    def genAdjustedCylinder(yCenter:Int, radius:Float, height:Int, block:BlockType, repl:Replacement)
    {
        genAdjustedCylinder(yCenter, 0, radius, height, block, repl)
    }

    def genAdjustedCylinder(yCenter:Int, offset:Int, radius:Float, height:Int, block:BlockType, repl:Replacement)
    {
        genCylinder(getCenteredAt(yCenter, offset), radius+girth, height, block, repl)
    }

    def canGrow = tree.canGrow(world, startX, startY, startZ, girth, height)

    def preGenerate()
    {
        height = determineHeight(tree.height, tree.heightVar)
        girth = determineGirth(tree.girth, tree.girthVar)
    }

    def determineGirth(required:Int, variation:Int) =
    {
        required+MathHelper.getRandomIntegerInRange(rand, -variation, variation)
    }

    def determineHeight(required:Int, variation:Int) =
    {
        required+MathHelper.getRandomIntegerInRange(rand, -variation, variation)
    }

    def getWood = new BlockTypeWood

    def getLeaf = new BlockTypeLeaf
}

class BlockTypeWood extends BlockType(0,0)
{
    override def setBlock(world:World, tree:ITreeGenData, x:Int, y:Int, z:Int) = tree.setWood(world, x, y, z)
}

class BlockTypeLeaf extends BlockType(0,0)
{
    override def setBlock(world:World, tree:ITreeGenData, x:Int, y:Int, z:Int) = tree.setLeaves(world, x, y, z)
}

abstract class GeneratorTreeBase(var tree:ITreeGenData) extends WorldGenCore
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

    def genTrunk(height:Int, girth:Int) {genTrunk(height, girth, 0)}
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