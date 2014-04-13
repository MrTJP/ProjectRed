package mrtjp.projectred.core;

import net.minecraft.block.Block;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeDirection;

public class BasicWireUtils
{
    public static boolean canPlaceWireOnSide(World w, int x, int y, int z, ForgeDirection side, boolean _default)
    {
        if (!w.blockExists(x, y, z))
            return _default;

        Block b = Block.blocksList[w.getBlockId(x, y, z)];
        if (b == null)
            return false;
        // Manual list of allowed blocks that wire can sit on.
        if (b == Block.glowStone || b == Block.pistonBase || b == Block.pistonStickyBase || b == Block.pistonMoving)
            return true;
        return b.isBlockSolidOnSide(w, x, y, z, side);
    }

    public static boolean canPlaceTorchOnBlock(World w, int x, int y, int z, boolean _default)
    {
        if (!w.blockExists(x, y, z))
            return _default;

        Block b = Block.blocksList[w.getBlockId(x, y, z)];
        if (b == null)
            return false;

        return b.canPlaceTorchOnTop(w, x, y, z);
    }
}
