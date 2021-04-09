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
import net.minecraft.block.state.pattern.BlockMatcher
import net.minecraft.util.math.BlockPos
import net.minecraft.world.gen.feature.WorldGenerator
import net.minecraft.world.{World, WorldType}
import net.minecraftforge.common.BiomeDictionary

import scala.collection.JavaConversions._

trait TGenerationLogic extends ISimpleStructureGen
{
    var name = ""

    var dimensionBlacklist = true
    var dimensions = Set[Int](-1, 1)

    var biomeBlacklist = true
    var biomes = Set[Set[BiomeDictionary.Type]]()

    var typeBlacklist = true
    var types = Set[WorldType](WorldType.FLAT)

    var resistance = 0
    var allowRetroGen = false

    override def genID = name

    def preFiltCheck(w:World, chunkX:Int, chunkZ:Int, rand:Random, isRetro:Boolean):Boolean =
    {
        if (isRetro && !allowRetroGen) return false
        if (dimensionBlacklist == dimensions.contains(w.provider.getDimension)) return false
        if (typeBlacklist == types.contains(w.getWorldInfo.getTerrainType)) return false
        if (resistance > 1 && rand.nextInt(resistance) != 0) return false
        true
    }

    def postFiltCheck(w:World, pos:BlockPos, rand:Random):Boolean =
    {
        val types = BiomeDictionary.getTypes(w.getBiome(pos))
        if (biomeBlacklist == biomes.contains(types)) return false
        true
    }

    @Deprecated
    def postFiltCheck(w:World, x:Int, z:Int, rand:Random):Boolean = postFiltCheck(w, new BlockPos(x, 0, z), rand)

    override def generate(w:World, chunkX:Int, chunkZ:Int, rand:Random, isRetro:Boolean):Boolean =
    {
        if (!preFiltCheck(w, chunkX, chunkZ, rand, isRetro)) return false
        generate_impl(w, chunkX, chunkZ, rand)
    }

    def generate_impl(w:World, chunkX:Int, chunkZ:Int, rand:Random):Boolean
}

trait TWorldGenerator extends WorldGenerator
{
    override def generate(w:World, rand:Random, pos:BlockPos):Boolean

    protected def canSetBlock(w:World, pos:BlockPos, material:Set[(Block, Int)]):Boolean =
    {
        if (material.isEmpty) return true
        val state = w.getBlockState(pos)
        val block = state.getBlock
        material.exists(pair => (pair._2 == -1 || pair._2 == block.getMetaFromState(state)) &&
                (block.isReplaceableOreGen(state, w, pos, BlockMatcher.forBlock(pair._1)) || block.isAssociatedBlock(pair._1)))
    }

    protected def setBlock(w:World, pos:BlockPos, cluster:Set[((Block, Int), Int)], material:Set[(Block, Int)]):Boolean =
    {
        if (canSetBlock(w, pos, material))
        {
            val genBlock = MathLib.weightedRandom(cluster, w.rand)
            val state = genBlock._1.getStateFromMeta(genBlock._2);
            w.setBlockState(pos, state, 2)
            true
        }
        else false
    }

    protected def setBlock(w:World, pos:BlockPos, cluster:(Block, Int), material:Set[(Block, Int)]) =
    {
        if (canSetBlock(w, pos, material))
        {
            w.setBlockState(pos, cluster._1.getStateFromMeta(cluster._2), 2)
            true
        }
        else false
    }
}

class GenLogicUniform extends TGenerationLogic
{
    var gen:WorldGenerator = null

    var attempts = 1
    var minY = 0
    var maxY = 0

    override def generate_impl(w:World, chunkX:Int, chunkZ:Int, rand:Random) =
    {
        var generated = false
        for (i <- 0 until attempts)
        {
            val x = chunkX*16+rand.nextInt(16)
            val y = minY+rand.nextInt(maxY-minY)
            val z = chunkZ*16+rand.nextInt(16)
            val pos = new BlockPos(x,y,z)
            if (postFiltCheck(w, pos, rand)) generated |= gen.generate(w, rand, pos)
        }
        generated
    }
}

class GenLogicSurface extends TGenerationLogic
{
    var gen:WorldGenerator = null

    var attempts = 1

    override def generate_impl(w:World, chunkX:Int, chunkZ:Int, rand:Random) =
    {
        var generated = false
        for (i <- 0 until attempts)
        {
            val x = chunkX*16+rand.nextInt(16)
            val z = chunkZ*16+rand.nextInt(16)
            var pos = new BlockPos(x, 0, z)
            if (postFiltCheck(w, pos, rand))
            {
                pos = WorldLib.findSurfaceHeight(w, pos)
                if (pos.getY > 0) generated |= gen.generate(w, rand, pos)
            }
        }
        generated
    }
}
*/
