package mrtjp.projectred.expansion;

import java.util.HashSet;
import java.util.PriorityQueue;

import mrtjp.projectred.core.BasicUtils;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.ISidedInventory;
import net.minecraft.inventory.InventoryLargeChest;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.tileentity.TileEntityChest;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;

public class BasicTubeUtils {

    public static ITubeInterface getTubeInterface(World w, BlockCoord b) {
        TileEntity tile = BasicUtils.getTileEntity(w, b, TileEntity.class);
        
        if (tile instanceof TileMultipart) {
            TMultiPart p = ((TileMultipart)tile).partMap(6);
            if (p instanceof ITubeInterface)
                return (ITubeInterface) p;
        }
        
        if (tile instanceof ITubeInterface)
            return (ITubeInterface) tile;
        
        return null;
    }
    
    public static boolean passTubeItemTo(World w, BlockCoord bc, TubeItem item) {
        ITubeInterface itube = getTubeInterface(w, bc);
        if (itube != null)
            return itube.addItem(item, item.direction^1);
        if (canAddToInventory(w, item.item, bc, item.direction^1))
            return addToInventory(w, item.item, bc, item.direction^1);
        return false;
    }
        
    public static boolean addToInventory(World world, ItemStack item, BlockCoord bc, int side) {
      return tryAddToInventory(world, item, bc, side, true);
    }
    
    public static boolean canAddToInventory(World world, ItemStack item, BlockCoord bc, int side) {
      return tryAddToInventory(world, item, bc, side, false);
    }
    
    public static boolean tryAddToInventory(World world, ItemStack item, BlockCoord bc, int side, boolean doAdd) {
        IInventory inv = getInventory(world, bc);
        if (inv == null)
            return false;
        
        int[] slots = new int[inv.getSizeInventory()];
        
        if ((inv instanceof ISidedInventory))
            slots = ((ISidedInventory)inv).getAccessibleSlotsFromSide(side);
        else
            for (int i = 0; i < inv.getSizeInventory(); i++)
                slots[i] = i;
        
        return addIntoInventory(inv, item, slots, doAdd);
    }

    public static boolean addIntoInventory(IInventory inv, ItemStack item, int[] slotsToCheck, boolean doAdd) {
        for (int n : slotsToCheck) {
            ItemStack stackInSlot = inv.getStackInSlot(n);
            if (stackInSlot == null) {
                if (!doAdd)
                    return true;

            } else if ((item.isItemEqual(stackInSlot)) && (ItemStack.areItemStackTagsEqual(item, stackInSlot))) {
                int freeSpace = Math.min(stackInSlot.getMaxStackSize(), inv.getInventoryStackLimit());

                freeSpace -= stackInSlot.stackSize;
                if (freeSpace > 0) {
                    int toAdd = Math.min(freeSpace, item.stackSize);
                    if (!doAdd)
                        return true;

                    stackInSlot.stackSize += toAdd;
                    inv.setInventorySlotContents(n, stackInSlot);
                    item.stackSize -= toAdd;
                    if (item.stackSize == 0)
                        return true;
                }
            }
        }
        if (!doAdd)
            return false;
        for (int n : slotsToCheck) {
            ItemStack invst = inv.getStackInSlot(n);
            if (invst == null) {
                if (inv.getInventoryStackLimit() >= item.stackSize) {
                    inv.setInventorySlotContents(n, item);
                    return true;
                }
                inv.setInventorySlotContents(n, item.splitStack(inv.getInventoryStackLimit()));
            }
        }

        return false;
    }
    
    public static IInventory getInventory(World world, BlockCoord wc) {
        IInventory inv = BasicUtils.getTileEntity(world, wc, IInventory.class);

        if (!(inv instanceof TileEntityChest))
            return inv;
        
        for (int i = 2; i < 6; i++) {
            TileEntityChest chest = BasicUtils.getTileEntity(world, wc.copy().offset(i), TileEntityChest.class);
            if (chest != null)
                return new InventoryLargeChest("Large chest", chest, inv);
        }
        return inv;
    }

    private static class ExitRouteFinder extends BasicTubeRouter {

        public ExitRouteFinder(World w, BlockCoord startingNode, TubeItem item) {
            super(w, startingNode, item);
        }
        
        @Override
        public boolean addToQuarry(BlockCoord addToQuarry, SearchQuarry parentQuarry, int directionFromStart, int directionFromParent, int newGCost) {
            if (canAddToInventory(w, this.item.item, addToQuarry, directionFromParent^1)) {
                SearchQuarry q = new SearchQuarry(addToQuarry, directionFromStart, directionFromParent, newGCost);
                q.isRouteCalculated = true;
                openList.add(q);
                return true;
            }
            
            ITubeInterface iti = getTubeInterface(w, addToQuarry);
            
            if (iti == null)
                return false;
            SearchQuarry q = new SearchQuarry(addToQuarry, directionFromStart, directionFromParent, newGCost+iti.getGCost());
            if (iti.isDestinationForItem(item, directionFromParent^1)) {
                q.isRouteCalculated = true;
                openList.add(q);
                return true;
            }
            if (!iti.canAcceptItem(item, directionFromParent^1)) return false;
            if (closedList.contains(q)) return false;
            closedList.add(q);
            openList.add(q);
            return false;
        }
    }
    
    private static class BasicTubeRouter {
        /**
         * The world that we are pathing in
         */
        World w;

        /**
         * The item we want to find a destination for.
         */
        TubeItem item;

        /**
         * The direction the item should travel from the tube its in. This is
         * changed when one with a lower GCost one is found.
         */
        SearchQuarry closestDestination;

        /**
         * The initial start of the search. Usually the tube that the item is
         * currently in.
         */
        BlockCoord startingNode;

        /**
         * The direction that the tube is currently traveling.
         * (travelingDirection^1) will not be included in the search because we
         * dont want to travel back to the direction the item came from.
         */
        int initialItemDirection;

        /**
         * List of nodes already looked at. Used when tubes loop. When we end up
         * at a tube that we already checked (i.e. if this list contains it) we
         * shouldnt worry about it.
         */
        HashSet<SearchQuarry> closedList = new HashSet<SearchQuarry>();

        /**
         * The queue that contains all the tubes we want to look at. Every time
         * a tube is checked, it is polled from the list, and any neighboring
         * tubes will be added. They search completes when this is empty.
         */
        PriorityQueue<SearchQuarry> openList = new PriorityQueue<SearchQuarry>();

        public BasicTubeRouter(World w, BlockCoord startingNode, TubeItem item) {
            this.w = w;
            this.startingNode = startingNode;
            this.initialItemDirection = item.direction;
            this.item = item;
        }

        /**
         * This actually adds the blocks to the open list if they should be
         * checked out. This must overridden to allow any routing, because this
         * will not resolve destinations on its own.
         * 
         * @param addToQuarry What to add.
         * @param parentQuarry The parent quarry that was adjacent to this and lead to this being added to the quarry.
         * @param directionFromStart The branch direction from the very first node.
         * @param directionFromParent Direction of this from the parent quarry.
         * @param newGCost calculated minimum incremented GCost, should be increased further if needed.
         * @return
         */
        public boolean addToQuarry(BlockCoord addToQuarry, SearchQuarry parentQuarry, int directionFromStart, int directionFromParent, int newGCost) {
            ITubeInterface t = getTubeInterface(w, addToQuarry);
            if (t == null)
                return false;
            if (!t.canAcceptItem(item, directionFromParent))
                return false;
            SearchQuarry q = new SearchQuarry(addToQuarry, directionFromStart, directionFromParent, newGCost);
            if (closedList.contains(q))
                return false;
            closedList.add(q);
            openList.add(q);
            return true;
        }

        /**
         * Adds up to 6 initial starting tubes, after which all those sides are
         * searched with standard A* algorithm.
         */
        public BasicTubeRouter findClosestDestination() {
            ITubeInterface startTube = getTubeInterface(w, startingNode);
            if (startTube != null) {
                for (int side = 0; side < 6; side++) {
                    if (side == (initialItemDirection ^ 1) || !startTube.maskConnects(side)) {
                        continue;
                    }
                    BlockCoord sidePipe = startingNode.copy().offset(side);
                    addToQuarry(sidePipe, null, side, side, 0);
                }
            }
            iterateThroughOpenList();
            return this;
        }

        private void iterateThroughOpenList() {
            while (openList.size() > 0) {
                SearchQuarry q = openList.poll();
                if (q.isRouteCalculated) {
                    this.closestDestination = q;
                    return;
                }
                ITubeInterface tube = getTubeInterface(w, startingNode);
                if (tube != null) {
                    for (int side = 0; side < 6; side++) {
                        if (side == (q.directionFromParent ^ 1) || !tube.maskConnects(side))
                            continue;

                        BlockCoord sidePipe = q.node.copy().offset(side);
                        addToQuarry(sidePipe, q, q.directionFromStartingNode, side, q.GCost + 4);
                    }
                }
            }
        }

        /**
         * Get the side the item should travel next after it has been figured
         * out. -1 for no destination.
         */
        public int getSideForBestRoute() {
            return closestDestination == null ? -1 : closestDestination.directionFromStartingNode;
        }
    }

    /**
     * A queued block in an A* algorithm. Has basic info needed for retracing.
     */
    private static class SearchQuarry implements Comparable {
        /**
         * The current tube we are looking at, the one this search quarry
         * pertains to.
         */
        public final BlockCoord node;

        /**
         * The direction the item has to travel from the starting pipe, should
         * this branch find a location.
         */
        public final int directionFromStartingNode;

        /**
         * The direction this quarry is in relative to its parent.
         * (directionFromParent^1) will not be included when adding more
         * quarries to the open list.
         */
        public final int directionFromParent;

        /**
         * The cost to route to this location, calculated upon creation of this
         * quarry by the actual path finder. Only GCost is used, F and H are
         * excluded.
         */
        public final int GCost;

        /**
         * If this route is considered valid and a destination has been found,
         * this flag is set to true, at which point, this directionFromStart is
         * set to be the valid direction the item should travel for the closest
         * route. This works because the queue is ordered from lowest to highest
         * GCost, the first route found is considered to be the lowest cost.
         */
        public boolean isRouteCalculated = false;

        public SearchQuarry(BlockCoord quarryNode, int directionFromStart, int directionFromParent, int GCost) {
            this.node = quarryNode;
            this.directionFromStartingNode = directionFromStart;
            this.directionFromParent = directionFromParent;
            this.GCost = GCost;
        }

        @Override
        public boolean equals(Object o) {
            if (o instanceof SearchQuarry) {
                SearchQuarry s = (SearchQuarry) o;
                return s.node.equals(node);
            }
            return false;
        }

        @Override
        public int compareTo(Object o) {
            return this.GCost - ((SearchQuarry) o).GCost;
        }
    }
}
