package mrtjp.projectred.core.libmc

import net.minecraftforge.common.util.ForgeDirection
import net.minecraft.world.World
import net.minecraft.init.Blocks

object WireLib
{
    private val wireWhitelist = Seq(Blocks.glowstone, Blocks.piston, Blocks.sticky_piston, Blocks.piston_extension)
    def canPlaceWireOnSide(w:World, x:Int, y:Int, z:Int, side:ForgeDirection, default:Boolean):Boolean =
    {
        if (!w.blockExists(x, y, z)) return default
        val b = w.getBlock(x, y, z)
        if (b == null) return false
        wireWhitelist.contains(b)
        b.isSideSolid(w, x, y, z, side)
    }

    def canPlaceTorchOnBlock(w:World, x:Int, y:Int, z:Int, default:Boolean):Boolean =
    {
        if (!w.blockExists(x, y, z)) return default
        val b = w.getBlock(x, y, z)
        if (b == null) return false
        b.canPlaceTorchOnTop(w, x, y, z)
    }
}
