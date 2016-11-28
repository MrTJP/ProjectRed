package mrtjp.projectred.core.libmc

import codechicken.lib.vec.Vector3
import net.minecraft.entity.item.EntityItem
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.init.Blocks
import net.minecraft.item.ItemStack
import net.minecraft.util.EnumFacing
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World

object PRLib
{
    def dropTowardsPlayer(w:World, pos:BlockPos, stack:ItemStack, p:EntityPlayer)
    {
        if (!w.isRemote && w.getGameRules.getBoolean("doTileDrops"))
        {
            val bpos = new Vector3(pos)
            val d = new Vector3(p.posX, p.posY, p.posZ).subtract(bpos).normalize()
            val vel = d.copy.multiply(8.0)
            val tpos = bpos.add(Vector3.center).add(d.copy.multiply(1.25))

            val item = new EntityItem(w, tpos.x, tpos.y, tpos.z, stack)
            item.motionX = vel.x*0.02
            item.motionY = vel.y*0.02
            item.motionZ = vel.z*0.02
            item.setPickupDelay(0)
            w.spawnEntityInWorld(item)
        }
    }

    private val wireWhitelist = Seq(Blocks.GLOWSTONE, Blocks.PISTON, Blocks.STICKY_PISTON, Blocks.PISTON_EXTENSION)
    def canPlaceWireOnSide(w:World, pos:BlockPos, side:Int):Boolean =
    {
        val state = w.getBlockState(pos)
        if (wireWhitelist.contains(state.getBlock)) return true
        state.isSideSolid(w, pos, EnumFacing.values()(side))
    }

    private val gateWhiteList = Seq(Blocks.GLASS)
    def canPlaceGateOnSide(w:World, pos:BlockPos, side:Int):Boolean =
    {
        if (canPlaceWireOnSide(w, pos, side)) return true

        val state = w.getBlockState(pos)
        if (gateWhiteList.contains(state.getBlock)) return true

        false
    }


    def canPlaceLight(w:World, pos:BlockPos, side:Int):Boolean =
    {
        if (canPlaceWireOnSide(w, pos, side)) return true
        if (side == 1) {
            val state = w.getBlockState(pos)
            return state.getBlock.canPlaceTorchOnTop(state, w, pos)
        }
        false
    }
}
