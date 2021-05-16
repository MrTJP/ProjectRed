/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.world

import mrtjp.core.math.PerlinNoiseGenerator
import net.minecraft.block.{Block, BlockState, GrassBlock, IGrowable}
import net.minecraft.entity.item.ItemEntity
import net.minecraft.item.ItemStack
import net.minecraft.tileentity.TileEntity
import net.minecraft.util.Direction
import net.minecraft.util.math.BlockPos
import net.minecraft.world._
import net.minecraft.world.biome.Biome.RainType
import net.minecraft.world.gen.Heightmap
import net.minecraftforge.common.IPlantable

object WorldLib
{
//    def getTileEntity[T](world:IBlockAccess, pos:BlockPos, clazz:Class[T]):T =
//    {
//        val tile = world.getTileEntity(pos)
//        if (!clazz.isInstance(tile)) null.asInstanceOf[T] else tile.asInstanceOf[T]
//    }

    def bulkBlockUpdate(world:World, pos:BlockPos, bl:Block)
    {
        for (a <- -3 to 3) for (b <- -3 to 3) for (c <- -3 to 3)
        {
            val md = (if (a < 0) -a else a) + (if (b < 0) -b else b) + (if (c < 0) -c else c)
            if (md <= 3)
            {
                val pos2 = new BlockPos(pos.getX+a, pos.getY+b, pos.getZ+c)
                world.neighborChanged(pos2, bl, pos)
            }
        }
    }

    def dropItem(world:World, pos:BlockPos, stack:ItemStack)
    {
        if (!world.isClientSide && world.getGameRules.getBoolean(GameRules.RULE_DOBLOCKDROPS))
        {
            val d = 0.7D
            val dx = world.random.nextFloat*d+(1.0D-d)*0.5D
            val dy = world.random.nextFloat*d+(1.0D-d)*0.5D
            val dz = world.random.nextFloat*d+(1.0D-d)*0.5D
            val item = new ItemEntity(world, pos.getX+dx, pos.getY+dy, pos.getZ+dz, stack)
            item.setPickUpDelay(10)
            world.addFreshEntity(item)
        }
    }

    def centerEject(w:World, pos:BlockPos, stack:ItemStack, dir:Int, vel:Double)
    {
        val pos2 = pos.relative(Direction.values()(dir))
        val item = new ItemEntity(w, pos2.getX+0.5D, pos2.getY+0.5D, pos2.getZ+0.5D, stack)


        item.setPickUpDelay(10)
        var motionY = 0D
        var motionZ = 0D
        var motionX = 0D
        dir match
        {
            case 0 => motionY = -vel
            case 1 => motionY =  vel
            case 2 => motionZ = -vel
            case 3 => motionZ =  vel
            case 4 => motionX = -vel
            case 5 => motionX =  vel
        }
        item.setDeltaMovement(motionX, motionY, motionZ)
        w.addFreshEntity(item)
    }

    def hasItem(state: BlockState) : Boolean = {
        val s = new ItemStack(state.getBlock)
        !s.isEmpty
    }

  /*  def isLeafType(world:World, pos:BlockPos, state:BlockState) =
        state.getBlock.isLeaves(state, world, pos) || (hasItem(state) && OreDictionary.getOreIDs(new ItemStack(state.getBlock)).contains(OreDictionary.getOreID("treeLeaves")))
    def isWoodType(world: World, pos:BlockPos, state:BlockState) =
        state.getBlock.isWood(world, pos) || (hasItem(state) && OreDictionary.getOreIDs(new ItemStack(state.getBlock)).contains(OreDictionary.getOreID("logWood")))
*/
    def isPlantType(world:World, pos:BlockPos, state:BlockState) = state.getBlock match
    {
        case b:IGrowable => !b.isInstanceOf[GrassBlock]
        case b:IPlantable => true
        case _ => false
    }

/*
    def isBlockSoft(world:World, pos:BlockPos, state:BlockState) =
        state.getBlock.isAir(state, world, pos) || state.getBlock.isReplaceable(world, pos) ||
                isLeafType(world, pos, state) || isPlantType(world, pos, state) ||
                    state.getBlock.canBeReplacedByLeaves(state, world, pos)
*/

/*    def isAssociatedTreeBlock(world:World, pos:BlockPos, state:BlockState) =
    {
        import net.minecraft.init.Blocks._
        Seq(LOG, LOG2, LEAVES, LEAVES2, VINE, COCOA).contains(state.getBlock) || isLeafType(world, pos, state) || isWoodType(world, pos, state)
    }*/

/*    def findSurfaceHeight(world:World, pos:BlockPos) =
    {
        var pos2 = world.getHeight(Heightmap.Type.WORLD_SURFACE, pos).up()
        do pos2 = pos2.down() while (pos2.getY >= 0 && {val b = world.getBlockState(pos2); isBlockSoft(world, pos2, b) || isAssociatedTreeBlock(world, pos2, b)})
        pos2
    }*/

    def isBlockTouchingAir(world:World, pos:BlockPos):Boolean =
    {
        for (s <- 0 until 6)
            if (world.isEmptyBlock(pos.relative(Direction.values()(s))))
                return true

        false
    }

/*    def isBlockUnderTree(world:World, pos:BlockPos):Boolean =
    {
        if (world.canBlockSeeSky(pos)) return false
        for (h <- pos.getY until world.getHeight)
        {
            val pos2 = pos.up(h)
            val b = world.getBlockState(pos2)
            if (isLeafType(world, pos2, b) || isAssociatedTreeBlock(world, pos2, b)) return true
        }
        false
    }*/

    def getSkyLightValue(world:World, pos:BlockPos) =
        world.getBrightness(LightType.SKY, pos)-world.getSkyDarken

    def getBlockLightValue(w:World, pos:BlockPos) = w.getBrightness(LightType.BLOCK, pos)

    private val noise = new PerlinNoiseGenerator(2576710L)
    def getWindSpeed(world:World, pos:BlockPos):Double =
    {
//        if (world.dimension.isSurfaceWorld) return 0.5D
        var nv = noise.noise(world.getDayTime*0.00000085D, 0, 0, 5, 7.5D, 5.0D, true)

        nv = math.max(0.0D, 1.6D*(nv-0.006D)+0.06D)*math.sqrt(pos.getY)/16.0D

        val bgb = world.getBiome(pos)
        if (bgb.getPrecipitation == RainType.RAIN)
            if (world.isThundering) return 2.5D*nv
            else if (world.isRaining) return 0.5D+0.5D*nv

        nv
    }
}
