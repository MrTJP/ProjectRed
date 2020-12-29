package mrtjp.projectred.core

import codechicken.lib.vec.Vector3
import net.minecraft.block.Blocks
import net.minecraft.entity.item.ItemEntity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.block.Block
import net.minecraft.item.ItemStack
import net.minecraft.util.Direction
import net.minecraft.util.math.BlockPos
import net.minecraft.world.{GameRules, World}

object PRLib
{
    def dropTowardsPlayer(w:World, pos:BlockPos, stack:ItemStack, p:PlayerEntity)
    {
        if (!w.isRemote && w.getGameRules.getBoolean(GameRules.DO_TILE_DROPS))
        {
            val bpos = Vector3.fromVec3i(pos)
            val d = new Vector3(p.getPositionVec).subtract(bpos).normalize()
            val vel = d.copy.multiply(8.0)
            val tpos = bpos.add(Vector3.CENTER).add(d.copy.multiply(1.25))

            val item = new ItemEntity(w, tpos.x, tpos.y, tpos.z, stack)
            vel.multiply(0.02)
            item.setVelocity(vel.x, vel.y, vel.z)
            item.setPickupDelay(0)
            w.addEntity(item)
        }
    }

    private val wireWhitelist = Seq(Blocks.GLOWSTONE, Blocks.PISTON, Blocks.STICKY_PISTON, Blocks.PISTON_HEAD)
    def canPlaceWireOnSide(w:World, pos:BlockPos, side:Direction):Boolean =
    {
        val state = w.getBlockState(pos)
        if (wireWhitelist.contains(state.getBlock)) return true
        Block.hasSolidSide(state, w, pos, side)
    }

    private val gateWhiteList = Seq(Blocks.GLASS)
    def canPlaceGateOnSide(w:World, pos:BlockPos, side:Direction):Boolean =
    {
        if (canPlaceWireOnSide(w, pos, side)) return true

        val state = w.getBlockState(pos)
        if (gateWhiteList.contains(state.getBlock)) return true

        false
    }


    def canPlaceLight(w:World, pos:BlockPos, side:Direction):Boolean =
    {
        if (canPlaceWireOnSide(w, pos, side)) return true
        if (side == Direction.UP) {
            return Block.hasEnoughSolidSide(w, pos, Direction.UP)
        }
        false
    }
}
