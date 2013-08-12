package mrtjp.projectred.transmission;

import net.minecraft.block.Block;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeDirection;

public class BasicWireUtils {

    public static final int FRONT = 0;
    public static final int BACK = 1;
    public static final int LEFT = 2;
    public static final int RIGHT = 3;
    
    public static boolean canPlaceWireOnSide(World w, int x, int y, int z, ForgeDirection side, boolean _default) {
        if (!w.blockExists(x, y, z)) {
            return _default;
        }

        Block b = Block.blocksList[w.getBlockId(x, y, z)];
        if (b == null)
            return false;
        // Manual list of allowed blocks that wire can sit on.
        if (b == Block.glowStone || b == Block.pistonBase || b == Block.pistonStickyBase || b == Block.pistonMoving)
            return true;
        return b.isBlockSolidOnSide(w, x, y, z, side);
    }
}
