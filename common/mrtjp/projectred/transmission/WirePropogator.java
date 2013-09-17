package mrtjp.projectred.transmission;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;
import java.util.Stack;

import mrtjp.projectred.core.CommandDebug;
import net.minecraft.block.Block;
import net.minecraft.block.BlockRedstoneWire;
import net.minecraft.world.World;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;
import codechicken.multipart.handler.MultipartProxy;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;

public class WirePropogator {
    /**
     * Used to prevent 2 disabled wires on opposite sides of a block from
     * keeping eachother on through a strong block signal.
     */
    private static Field wiresProvidePower = BlockRedstoneWire.class.getDeclaredFields()[0];
    public static boolean redwiresProvidePower = true;
    private static ThreadLocal<Boolean> redwiresConnectable = new ThreadLocal<Boolean>();
    
    private static TMultiPart notApart = new TMultiPart() {
        @Override
        public String getType() {
            return null;
        }
    };
    
    static {
        try {
            wiresProvidePower.setAccessible(true);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
        
    public static void setWiresProvidePower(boolean b) {
        try {
            wiresProvidePower.setBoolean(Block.redstoneWire, b);
        } catch (Throwable t) {}
    }
    
    public static boolean redwiresConnectable() {
        Boolean b = redwiresConnectable.get();
        return b == null ? true : b;
    }
    
    public static void setRedwiresConnectable(boolean b) {
        redwiresConnectable.set(b);
    }
    
    private static class PropogationRun {
        public class Propogation {
            public IWirePart part;
            public TMultiPart prev;
            public int mode;
            
            public Propogation(IWirePart part, TMultiPart prev, int mode) {
                this.part = part;
                this.prev = prev;
                this.mode = mode;
            }

            public void propogate() {
                part.updateAndPropogate(prev, mode);
            }
        }
        
        private World world;
        private PropogationRun parent;
        
        private TMultiPart lastCaller;
        private int count;
        private int recalcs;
        
        private Multimap<TileMultipart, TMultiPart> partChanges = HashMultimap.create();
        private HashSet<BlockCoord> neighborChanges = new HashSet<BlockCoord>();
        private LinkedList<Propogation> propogationList = new LinkedList<Propogation>();
        private LinkedList<Propogation> analogDrops = new LinkedList<Propogation>();
        
        public void clear() {
            partChanges.clear();
            neighborChanges.clear();
            count = 0;
            recalcs = 0;
            lastCaller = null;
            
            reusableRuns.add(this);
        }
        
        public void finish() {
            currentRun = null;
            
            if(partChanges.isEmpty() && neighborChanges.isEmpty()) {
                finishing = parent;
                clear();
                return;
            }
            
            finishing = this;

            if(CommandDebug.WIRE_READING)
                System.out.println(""+count+" propogations, "+partChanges.size()+" part changes, "+neighborChanges.size()+" block updates");
            
            for(Entry<TileMultipart, Collection<TMultiPart>> entry : partChanges.asMap().entrySet()) {
                Collection<TMultiPart> parts = entry.getValue();
                for(TMultiPart part : parts)
                    ((IWirePart)part).onSignalUpdate();
                
                entry.getKey().multiPartChange(parts);
            }
            
            int blockID = ((Block)MultipartProxy.block()).blockID;
            for(BlockCoord b : neighborChanges)
                world.notifyBlockOfNeighborChange(b.x, b.y, b.z, blockID);
            
            finishing = parent;
            
            if(CommandDebug.WIRE_READING)
                System.out.println(""+recalcs+" recalculations");
            
            clear();
        }
        
        public void start(PropogationRun parent, World world) {
            this.world = world;
            this.parent = parent;
            
            currentRun = this;
            
            runLoop();
        }
        
        private void runLoop() {
            do {
                List<Propogation> list = propogationList;
                propogationList = new LinkedList<Propogation>();
                
                for(Propogation p : list)
                    p.propogate();
                
                if(propogationList.isEmpty() && !analogDrops.isEmpty()) {
                    propogationList = analogDrops;
                    analogDrops = new LinkedList<Propogation>();
                }
            }
            while(!propogationList.isEmpty());
            finish();
        }
        
        public void add(IWirePart part, TMultiPart prev, int mode) {
            if(prev != lastCaller) {
                lastCaller = prev;
                count++;
            }
            
            propogationList.add(new Propogation(part, prev, mode));
        }

        public void addAnalogDrop(IWirePart part) {
            analogDrops.add(new Propogation(part, notApart, IWirePart.RISING));
        }
    }
    
    private static Stack<PropogationRun> reusableRuns = new Stack<PropogationRun>();
    private static PropogationRun currentRun = null;
    private static PropogationRun finishing = null;
    
    private static PropogationRun getRun() {
        if(reusableRuns.isEmpty())
            return new PropogationRun();
        
        return reusableRuns.pop();
    }
    
    public static void addNeighborChange(BlockCoord pos) {
        currentRun.neighborChanges.add(pos);
    }
    
    public static void addPartChange(TMultiPart part) {
        currentRun.partChanges.put(part.tile(), part);
    }
    
    public static void logCalculation() {
        if(finishing != null)
            finishing.recalcs++;
    }
    
    public static void propogateTo(IWirePart part, TMultiPart prev, int mode) {
        PropogationRun p = currentRun;
        if(p == null)
            p = getRun();
        p.add(part, prev, mode);
        if(currentRun != p) {
            if(currentRun != null)
                throw new RuntimeException("Report this to ProjectRed developers");
            
            p.start(finishing, part.world());
        }
    }

    public static void propogateTo(IWirePart part, int mode) {
        propogateTo(part, notApart, mode);
    }
    
    public static void propogateAnalogDrop(IWirePart part) {
        currentRun.addAnalogDrop(part);
    }
}
