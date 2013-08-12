package mrtjp.projectred.transmission;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashSet;

import net.minecraft.block.Block;
import net.minecraft.block.BlockRedstoneWire;
import net.minecraft.world.World;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.handler.MultipartProxy;

public class WirePropogator {
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
    
    public static void setWiresProvidePower(boolean b) {
        try {
            wiresProvidePower.setBoolean(Block.redstoneWire, b);
        } catch (Throwable t) {
        }
    }

    public static HashSet<TMultiPart> partChanges = new HashSet<TMultiPart>();
    public static HashSet<BlockCoord> neighborChanges = new HashSet<BlockCoord>();
    public static int propogatingChanges = 0;
    
    public static void beginPropogating() {
        propogatingChanges++;
    }
    
    public static void endPropogating(World world) {
        if(propogatingChanges == 1) {
            while(!partChanges.isEmpty() || !neighborChanges.isEmpty()) {
                ArrayList<TMultiPart> _partChanges = new ArrayList<TMultiPart>(partChanges);
                ArrayList<BlockCoord> _neighborChanges = new ArrayList<BlockCoord>(neighborChanges);
                partChanges.clear();
                neighborChanges.clear();
                
                for(TMultiPart part : _partChanges)
                    part.tile().notifyPartChange(part);
                int blockID = ((Block)MultipartProxy.block()).blockID;
                for(BlockCoord b : _neighborChanges)
                    world.notifyBlockOfNeighborChange(b.x, b.y, b.z, blockID);
            }
        }
        propogatingChanges--;
    }
    
    
}
