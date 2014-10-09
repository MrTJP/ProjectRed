package mrtjp.projectred.core.libmc

import codechicken.microblock.HollowMicroblock
import net.minecraft.init.Blocks
import net.minecraft.world.World
import net.minecraftforge.common.util.ForgeDirection

object WireLib
{
    private val wireWhitelist = Seq(Blocks.glowstone, Blocks.piston, Blocks.sticky_piston, Blocks.piston_extension)
    def canPlaceWireOnSide(w:World, x:Int, y:Int, z:Int, side:Int, default:Boolean):Boolean =
    {
        if (!w.blockExists(x, y, z)) return default
        val b = w.getBlock(x, y, z)
        if (b == null) return false
        wireWhitelist.contains(b)
        b.isSideSolid(w, x, y, z, ForgeDirection.getOrientation(side))
    }

    def canPlaceTorchOnBlock(w:World, x:Int, y:Int, z:Int, default:Boolean):Boolean =
    {
        if (!w.blockExists(x, y, z)) return default
        val b = w.getBlock(x, y, z)
        if (b == null) return false
        b.canPlaceTorchOnTop(w, x, y, z)
    }

    def canPlaceLight(w:World, x:Int, y:Int, z:Int, side:Int, canFloat:Boolean):Boolean =
    {
        if (canFloat) return true
        if (WireLib.canPlaceWireOnSide(w, x, y, z, side, false)) return true
        if (side == 0 && WireLib.canPlaceTorchOnBlock(w, x, y, z, false)) return true

        val part = PRLib.getMultiPart(w, x, y, z, side)
        if (part.isInstanceOf[HollowMicroblock]) return true

        false
    }
}