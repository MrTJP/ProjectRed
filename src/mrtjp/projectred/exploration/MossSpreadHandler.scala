/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.exploration

import java.util.Random

import codechicken.lib.vec.BlockCoord
import mrtjp.core.world.IBlockEventHandler
import net.minecraft.block.Block
import net.minecraft.init.Blocks
import net.minecraft.world.World

object MossSpreadHandler extends IBlockEventHandler
{
    override def onBlockUpdate(w:World, x:Int, y:Int, z:Int, b:Block) =
    {
        b match
        {
            case Blocks.mossy_cobblestone => doMossSpread(w, x, y, z, w.rand)
            case Blocks.stonebrick => w.getBlockMetadata(x, y, z) match
            {
                case 0 => crackFromHeat(w, x, y, z, w.rand)
                case 1 => doMossSpread(w, x, y, z, w.rand)
                case _ =>
            }
            case _ =>
        }
    }

    private def crackFromHeat(w:World, x:Int, y:Int, z:Int, r:Random)
    {
        val bc = new BlockCoord(x, y, z)
        if (isBlockWet(w, bc) && isBlockHot(w, bc))
            if (r.nextInt(3) == 0) w.setBlock(x, y, z, Blocks.stonebrick, 2, 3)
    }

    private def doMossSpread(w:World, x:Int, y:Int, z:Int, r:Random)
    {
        val bc1 = new BlockCoord(x, y, z)
        if (!isBlockTouchingAir(w, bc1) || w.canBlockSeeTheSky(x, y+1, z)) return

        for (i <- 0 until 6)
        {
            val bc = bc1.copy.offset(i)
            val b = w.getBlock(bc.x, bc.y, bc.z)
            val meta = w.getBlockMetadata(bc.x, bc.y, bc.z)

            if (!w.canBlockSeeTheSky(bc.x, bc.y, bc.z))
            {
                if (b == Blocks.cobblestone)
                {
                    if (isBlockWet(w, bc) && isBlockTouchingAir(w, bc))
                        if (r.nextInt(3) == 0) w.setBlock(bc.x, bc.y, bc.z, Blocks.mossy_cobblestone, 0, 3)
                }
                else if (b == Blocks.stonebrick && meta == 2)
                {
                    if (isBlockWet(w, bc) && isBlockTouchingAir(w, bc))
                        if (r.nextInt(3) == 0) w.setBlock(bc.x, bc.y, bc.z, Blocks.stonebrick, 1, 3)
                }
            }
        }
    }

    private def isBlockTouchingAir(w:World, b:BlockCoord) = ncheck(w, b){_ == Blocks.air}

    private def isBlockWet(w:World, b:BlockCoord) = ncheck(w, b){b => b == Blocks.flowing_water || b == Blocks.water}

    private def isBlockHot(w:World, b:BlockCoord) = ncheck(w, b){b => b == Blocks.flowing_lava || b == Blocks.lava}

    val wetSources = Set(Blocks.flowing_water, Blocks.water)
    val heatSources = Set(Blocks.flowing_lava, Blocks.lava)
    val wetAndHot = wetSources++heatSources

    private def ncheck(w:World, b:BlockCoord)(f:Block => Boolean):Boolean =
    {
        for (i <- 0 until 6)
        {
            val bc = b.copy.offset(i)
            val block = w.getBlock(bc.x, bc.y, bc.z)
            if (f(block)) return true
        }
        false
    }
}