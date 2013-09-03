package mrtjp.projectred.transmission;

import mrtjp.projectred.core.BasicUtils;
import net.minecraft.block.Block;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.PartMap;
import codechicken.multipart.TileMultipart;

public class BasicWireUtils {
    
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

    public static boolean canConnectThroughCorner(World world, BlockCoord pos, int side1, int side2) {
        if(world.isAirBlock(pos.x, pos.y, pos.z))
            return true;
        
        TileMultipart t = BasicUtils.getMultipartTile(world, pos);
        if(t != null)
            return t.partMap(side1) == null && t.partMap(side2) == null && t.partMap(PartMap.edgeBetween(side1, side2)) == null;
        
        return false;
    }
}
