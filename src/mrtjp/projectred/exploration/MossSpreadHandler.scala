/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.exploration

import java.util.Random

import mrtjp.core.world.IBlockEventHandler
import net.minecraft.block.BlockStoneBrick.EnumType
import net.minecraft.block.BlockStoneBrick
import net.minecraft.block.state.IBlockState
import net.minecraft.init.Blocks
import net.minecraft.util.EnumFacing
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World

object MossSpreadHandler extends IBlockEventHandler
{
    override def onBlockUpdate(w:World, pos:BlockPos, state:IBlockState) =
    {
        state.getBlock match
        {
            case Blocks.MOSSY_COBBLESTONE => doMossSpread(w, pos, w.rand)
            case Blocks.STONEBRICK => state.getValue(BlockStoneBrick.VARIANT) match
            {
                case EnumType.DEFAULT => crackFromHeat(w, pos, w.rand)
                case EnumType.MOSSY => doMossSpread(w, pos, w.rand)
                case _ =>
            }
            case _ =>
        }
    }

    private def crackFromHeat(w:World, pos:BlockPos, r:Random)
    {
        if (isBlockWet(w, pos) && isBlockHot(w, pos))
            if (r.nextInt(3) == 0) w.setBlockState(pos, Blocks.STONEBRICK.getDefaultState.withProperty(BlockStoneBrick.VARIANT, EnumType.CRACKED), 3)
    }

    private def doMossSpread(w:World, pos:BlockPos, r:Random)
    {
        if (!isBlockTouchingAir(w, pos) || w.canSeeSky(pos.up())) return

        for (i <- 0 until 6)
        {
            val p = pos.offset(EnumFacing.values()(i))
            val b = w.getBlockState(p)

            if (!w.canSeeSky(p))
            {
                if (b.getBlock == Blocks.COBBLESTONE)
                {
                    if (isBlockWet(w, p) && isBlockTouchingAir(w, p))
                        if (r.nextInt(3) == 0) w.setBlockState(p, Blocks.MOSSY_COBBLESTONE.getDefaultState, 3)
                }
                else if (b == Blocks.STONEBRICK && b.getValue(BlockStoneBrick.VARIANT) == EnumType.CRACKED)
                {
                    if (isBlockWet(w, p) && isBlockTouchingAir(w, p))
                        if (r.nextInt(3) == 0) w.setBlockState(p, Blocks.STONEBRICK.getDefaultState.withProperty(BlockStoneBrick.VARIANT, EnumType.MOSSY), 3)
                }
            }
        }
    }

    private def isBlockTouchingAir(w:World, b:BlockPos) = ncheck(w, b){_.getBlock == Blocks.AIR}

    private def isBlockWet(w:World, b:BlockPos) = ncheck(w, b){state => state.getBlock == Blocks.FLOWING_WATER || state.getBlock == Blocks.WATER}

    private def isBlockHot(w:World, b:BlockPos) = ncheck(w, b){state => state.getBlock == Blocks.FLOWING_LAVA || state.getBlock == Blocks.LAVA}

    val wetSources = Set(Blocks.FLOWING_WATER, Blocks.WATER)
    val heatSources = Set(Blocks.FLOWING_LAVA, Blocks.LAVA)
    val wetAndHot = wetSources++heatSources

    private def ncheck(w:World, b:BlockPos)(f:IBlockState => Boolean):Boolean =
    {
        for (i <- 0 until 6)
        {
            val block = w.getBlockState(b)
            if (f(block)) return true
        }
        false
    }
}
