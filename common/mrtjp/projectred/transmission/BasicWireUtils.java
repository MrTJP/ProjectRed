package mrtjp.projectred.transmission;

import java.lang.reflect.Field;

import mrtjp.projectred.core.BasicUtils;
import net.minecraft.block.Block;
import net.minecraft.block.BlockRedstoneWire;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.PartMap;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;

public class BasicWireUtils {

    public static final int FRONT = 0;
    public static final int BACK = 1;
    public static final int LEFT = 2;
    public static final int RIGHT = 3;



    /**
     * Used to prevent 2 disabled wires on opposite sides of a block from
     * keeping eachother on through a strong block signal.
     */
    private static Field wiresProvidePower = BlockRedstoneWire.class.getDeclaredFields()[0];
    static {
        try {
            wiresProvidePower.setAccessible(true);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public static boolean wiresProvidePower() {
        try {
            return wiresProvidePower.getBoolean(Block.redstoneWire);
        } catch (Throwable t) {
            return false;
        }
    }

    public static boolean canPlaceWireOnSide(World w, int x, int y, int z, ForgeDirection side, boolean _default) {
        if (!w.blockExists(x, y, z)) {
            return _default;
        }

        Block b = Block.blocksList[w.getBlockId(x, y, z)];
        if (b == null)
            return false;
        // Manual list of allowed blocks that wire can sit on.
        if (b == Block.glowStone || b == Block.pistonBase || b == Block.pistonStickyBase || b == Block.pistonMoving || b == Block.glass)
            return true;
        return b.isBlockSolidOnSide(w, x, y, z, side);
    }
}
