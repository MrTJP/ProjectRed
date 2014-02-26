package mrtjp.projectred.transportation;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.INeighborTileChange;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;
import mrtjp.projectred.api.IScrewdriver;
import mrtjp.projectred.api.ISpecialLinkState;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.inventory.InventoryWrapper;
import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.core.utils.ItemKeyStack;
import mrtjp.projectred.core.utils.Pair2;
import mrtjp.projectred.transportation.RoutedPayload.SendPriority;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.IInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.Icon;
import net.minecraft.util.MovingObjectPosition;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeDirection;

import java.util.*;
import java.util.concurrent.PriorityBlockingQueue;

public class RoutedJunctionPipePart extends BasicPipePart implements IWorldRouter, IRouteLayer, IWorldRequester, IInventoryProvider, INeighborTileChange
{
    public int linkMap;

    public Router router;
    public String routerID;
    public final Object routerIDLock = new Object();

    public int inOutSide = 0;

    public boolean needsWork = true;
    public boolean firstTick = true;

    public static int pipes = 0;
    public int searchDelay = 0;

    private LinkedList<RoutedPayload> sendQueue = new LinkedList<RoutedPayload>();
    private PriorityBlockingQueue<Pair2<RoutedPayload, Integer>> transitQueue = new PriorityBlockingQueue<Pair2<RoutedPayload, Integer>>(10, new TransitComparator());
    private LinkedList<RoutedPayload> swapQueue = new LinkedList<RoutedPayload>();

    private class TransitComparator implements Comparator<Pair2<RoutedPayload, Integer>>
    {
        @Override
        public int compare(Pair2<RoutedPayload, Integer> o1, Pair2<RoutedPayload, Integer> o2)
        {
            int c = o2.getValue2()-o1.getValue2();
            if (c == 0)
                c = o2.getValue1().payload.compareTo(o1.getValue1().payload);
            return c;
        }
    }

    public RoutedJunctionPipePart()
    {
        pipes++;
        searchDelay = pipes%Configurator.detectionFrequency;
    }

    @Override
    public Router getRouter()
    {
        if (needsWork)
            return null;

        if (router == null)
            synchronized (routerIDLock)
            {
                UUID id = null;
                if (routerID != null && !routerID.isEmpty())
                    id = UUID.fromString(routerID);
                router = RouterServices.instance.getOrCreateRouter(id, this);
            }

        return router;
    }

    @Override
    public void itemEnroute(RoutedPayload r)
    {
        transitQueue.add(new Pair2<RoutedPayload, Integer>(r, 200));
    }

    @Override
    public void itemArrived(RoutedPayload r)
    {
        removeFromTransitQueue(r);

        trackedItemReceived(r.payload);
    }

    private void removeFromTransitQueue(RoutedPayload r)
    {
        Iterator<Pair2<RoutedPayload, Integer>> it = transitQueue.iterator();
        while (it.hasNext())
        {
            Pair2<RoutedPayload, Integer> pair = it.next();
            if (pair.getValue1() == r)
            {
                it.remove();
                break;
            }
        }
    }

    private void tickTransitQueue()
    {
        Iterator<Pair2<RoutedPayload, Integer>> it = transitQueue.iterator();
        while (it.hasNext())
        {
            Pair2<RoutedPayload, Integer> pair = it.next();
            Integer val = pair.getValue2();
            if (val == null)
                val = 0;
            val--;
            if (val < 0)
                it.remove();
            else
                pair.setValue2(val);
        }
    }

    protected int countInTransit(ItemKey key)
    {
        int count = 0;
        for (Pair2<RoutedPayload, Integer> pair : transitQueue)
        {
            Integer val = pair.getValue2();
            ItemKeyStack stack = pair.getValue1().payload;
            if (stack.key().equals(key))
                count += val;
        }
        return count;
    }

    @Override
    public void queueStackToSend(ItemStack stack, int dirOfExtraction, SyncResponse path)
    {
        queueStackToSend(stack, dirOfExtraction, path.priority, path.responder);
    }

    @Override
    public void queueStackToSend(ItemStack stack, int dirOfExtraction, SendPriority priority, int destination)
    {
        ItemKeyStack stack2 = ItemKeyStack.get(stack);
        RoutedPayload r = pollFromSwapQueue(stack2);

        if (r == null)
        {
            r = new RoutedPayload(stack2);

            r.input = ForgeDirection.getOrientation(dirOfExtraction);
            r.setPriority(priority);
            r.setDestination(destination);
        }
        sendQueue.addLast(r);
    }

    private void dispatchQueuedPayload(RoutedPayload r)
    {
        injectPayload(r, r.input);
        Router dest = RouterServices.instance.getRouter(r.destinationIP);
        if (dest != null)
        {
            IWorldRouter wr = dest.getParent();
            wr.itemEnroute(r);
            RouteFX.spawnType1(RouteFX.color_sync, 8, new BlockCoord(wr.getContainer().tile()), world());
        }

        RouteFX.spawnType1(RouteFX.color_send, 8, new BlockCoord(tile()), world());
    }

    public void queueSwapSendItem(RoutedPayload r)
    {
        swapQueue.add(r);
    }

    private RoutedPayload pollFromSwapQueue(ItemKeyStack stack)
    {
        Iterator<RoutedPayload> it = swapQueue.iterator();
        while (it.hasNext())
        {
            RoutedPayload r = it.next();
            if (r.payload.equals(stack))
            {
                it.remove();
                return r;
            }
        }

        return null;
    }

    @Override
    public SyncResponse getLogisticPath(ItemKey stack, BitSet exclusions, boolean excludeStart)
    {
        LogisticPathFinder p = new LogisticPathFinder(getRouter(), stack);
        if (exclusions != null)
            p.setExclusions(exclusions);
        p.setExcludeSource(excludeStart).findBestResult();
        return p.getResult();
    }

    @Override
    public SyncResponse getSyncResponse(ItemKey item, SyncResponse rival)
    {
        return null;
    }

    @Override
    public final void update()
    {
        if (needsWork)
        {
            needsWork = false;
            coldBoot();
            return;
        }
        if (!world().isRemote)
            getRouter().update(world().getTotalWorldTime()%Configurator.detectionFrequency == searchDelay || firstTick);

        super.update();
        firstTick = false;

        // Dispatch queued items
        while (!sendQueue.isEmpty())
            dispatchQueuedPayload(sendQueue.removeFirst());

        // Manage transit queue
        tickTransitQueue();

        if (world().isRemote)
            updateClient();
        else
            updateServer();
    }

    protected void updateServer()
    {
    }

    protected void updateClient()
    {
        if (world().getTotalWorldTime()%(Configurator.detectionFrequency*20) == searchDelay || firstTick)
            for (int i = 0; i < 6; i++)
                if ((linkMap&1<<i) != 0)
                    RouteFX.spawnType3(RouteFX.color_blink, 1, i, getCoords(), world());
    }

    private void coldBoot()
    {
        if (!world().isRemote)
            getRouter();
    }

    @Override
    public boolean needsWork()
    {
        return needsWork;
    }

    @Override
    public boolean refreshState()
    {
        if (world().isRemote)
            return false;
        int link = 0;
        for (ForgeDirection d : ForgeDirection.VALID_DIRECTIONS)
            if (getRouter().LSAConnectionExists(d))
                link |= 1<<d.ordinal();
        if (linkMap != link)
        {
            linkMap = link;
            sendLinkMapUpdate();
            return true;
        }
        return false;
    }

    @Override
    public RoutedJunctionPipePart getContainer()
    {
        return this;
    }

    @Override
    public void endReached(RoutedPayload r)
    {
        if (!world().isRemote)
            if (!maskConnects(r.output.ordinal()) || !passToNextPipe(r))
            {
                BlockCoord bc = new BlockCoord(tile()).offset(r.output.ordinal());

                // Injection to special link state
                TileEntity tile = BasicUtils.getTileEntity(world(), bc, TileEntity.class);
                ISpecialLinkState state = LSPathFinder.getLinkState(tile);
                if (state != null && tile instanceof IInventory)
                {
                    TileEntity dest = state.getLink(tile);
                    IInventory inv = (IInventory)tile;

                    if (dest instanceof TileMultipart)
                    {
                        TMultiPart part = ((TileMultipart)dest).partMap(6);
                        if (part instanceof RoutedJunctionPipePart)
                        {
                            RoutedJunctionPipePart pipe = (RoutedJunctionPipePart)part;
                            InventoryWrapper w = InventoryWrapper.wrapInventory(inv).setSlotsFromSide(r.output.getOpposite().ordinal());
                            int room = w.getRoomAvailableForItem(r.payload.key());
                            if (room >= r.payload.stackSize)
                            {
                                w.injectItem(r.payload.makeStack(), true);
                                pipe.queueSwapSendItem(r);
                                return;
                            }
                            else
                            {
                                bounceStack(r);
                                return;
                            }
                        }
                    }
                }

                // Injection to inventories
                IInventory inv = InventoryWrapper.getInventory(world(), bc);
                if (inv != null)
                {
                    InventoryWrapper w = InventoryWrapper.wrapInventory(inv).setSlotsFromSide(r.output.getOpposite().ordinal());
                    r.payload.stackSize -= w.injectItem(r.payload.makeStack(), true);
                }
                // Bounce
                if (r.payload.stackSize > 0)
                    bounceStack(r);
            }
    }

    @Override
    public void read(MCDataInput packet, int switch_key)
    {
        if (switch_key == 51)
            handleLinkMap(packet);
        else if (switch_key == 15)
        {
            inOutSide = packet.readUByte();
            tile().markRender();
        }
        else
            super.read(packet, switch_key);
    }

    private void handleLinkMap(MCDataInput packet)
    {
        int old = linkMap;
        linkMap = packet.readUByte();

        int high = linkMap&~old;
        int low = ~linkMap&old;

        BlockCoord bc = getCoords();
        for (int i = 0; i < 6; i++)
        {
            if ((high&1<<i) != 0)
                RouteFX.spawnType3(RouteFX.color_linked, 1, i, bc, world());
            if ((low&1<<i) != 0)
                RouteFX.spawnType3(RouteFX.color_unlinked, 1, i, bc, world());
        }

        tile().markRender();
    }

    public void sendLinkMapUpdate()
    {
        getWriteStream().writeByte(51).writeByte(linkMap);
    }

    @Override
    public String getType()
    {
        return "pr_rbasic";
    }

    @Override
    public void onRemoved()
    {
        super.onRemoved();
        pipes = Math.max(pipes-1, 0);
        Router r = getRouter();
        if (r != null)
            r.decommission();
    }

    @Override
    public void save(NBTTagCompound tag)
    {
        super.save(tag);
        synchronized (routerIDLock)
        {
            if (routerID == null || routerID.isEmpty())
                if (router != null)
                    routerID = getRouter().getID().toString();
                else
                    routerID = UUID.randomUUID().toString();
        }
        tag.setString("rid", routerID);
        tag.setByte("io", (byte)inOutSide);
    }

    @Override
    public void load(NBTTagCompound tag)
    {
        super.load(tag);
        synchronized (routerIDLock)
        {
            routerID = tag.getString("rid");
        }
        inOutSide = tag.getByte("io");
    }

    @Override
    public void writeDesc(MCDataOutput packet)
    {
        super.writeDesc(packet);
        packet.writeByte(linkMap);
        packet.writeByte(inOutSide);
    }

    @Override
    public void readDesc(MCDataInput packet)
    {
        super.readDesc(packet);
        linkMap = packet.readUByte();
        inOutSide = packet.readUByte();
    }

    @Override
    public void onNeighborChanged()
    {
        super.onNeighborChanged();
        shiftOrientation(false);
    }

    @Override
    public void onPartChanged(TMultiPart p)
    {
        super.onPartChanged(p);
        shiftOrientation(false);
    }

    @Override
    public void onAdded()
    {
        super.onAdded();
        shiftOrientation(false);
    }

    public void sendOrientUpdate()
    {
        tile().getWriteStream(this).writeByte(15).writeByte(inOutSide);
    }

    @Override
    public boolean connect(int absDir)
    {
        if (super.connect(absDir))
            return true;
        BlockCoord bc = new BlockCoord(tile()).offset(absDir);
        TileEntity t = BasicUtils.getTileEntity(world(), bc, TileEntity.class);

        return t instanceof IInventory;
    }

    @Override
    public boolean activate(EntityPlayer player, MovingObjectPosition hit, ItemStack item)
    {
        if (super.activate(player, hit, item))
            return true;

        if (item != null && item.getItem() instanceof IScrewdriver)
        {
            shiftOrientation(true);
            ((IScrewdriver)item.getItem()).damageScrewdriver(world(), player);
            return true;
        }

        return false;
    }

    public void shiftOrientation(boolean force)
    {
        if (world().isRemote)
            return;
        boolean invalid = force || !maskConnects(inOutSide) || !(BasicUtils.getTileEntity(world(), new BlockCoord(tile()).offset(inOutSide), TileEntity.class) instanceof IInventory);
        if (!invalid)
            return;

        boolean found = false;
        int oldSide = inOutSide;

        for (int i = 0; i < 6; ++i)
        {
            inOutSide = (inOutSide+1)%6;

            if (!maskConnects(inOutSide))
                continue;

            BlockCoord bc = new BlockCoord(tile()).offset(inOutSide);
            TileEntity t = BasicUtils.getTileEntity(world(), bc, TileEntity.class);
            if (t instanceof IInventory)
            {
                found = true;
                break;
            }
        }

        if (!found)
            inOutSide = -1;

        if (oldSide != inOutSide)
            sendOrientUpdate();
    }

    @Override
    public IInventory getInventory()
    {
        if (inOutSide < 0 || inOutSide > 5)
            return null;

        return InventoryWrapper.getInventory(world(), new BlockCoord(tile()).offset(inOutSide));
    }

    @Override
    public int getInterfacedSide()
    {
        return inOutSide < 0 || inOutSide > 5 ? -1 : inOutSide^1;
    }

    @Override
    public Icon getIcon(int side)
    {
        Icon[] array = EnumPipe.ROUTEDJUNCTION.sprites;
        int ind = side == inOutSide ? 2 : 0;

        if ((linkMap&1<<side) != 0)
            return array[1+ind];
        else
            return array[2+ind];
    }

    @Override
    public void onNeighborTileChanged(int side, boolean weak)
    {
    }

    @Override
    public boolean weakTileChanges()
    {
        return false;
    }

    @Override
    public void resolveDestination(RoutedPayload r)
    {
        if (needsWork())
            return;

        int color = -1;
        r.output = ForgeDirection.UNKNOWN;

        // Reroute item if it needs one
        r.refreshIP();
        if (r.destinationIP < 0 || r.destinationIP >= 0 && r.hasArrived)
        {
            r.resetTrip();
            LogisticPathFinder f = new LogisticPathFinder(getRouter(), r.payload.key()).setExclusions(r.travelLog).findBestResult();
            if (f.getResult() != null)
            {
                r.setDestination(f.getResult().responder).setPriority(f.getResult().priority);
                color = RouteFX.color_route;
            }
        }
        r.refreshIP();

        // Deliver item, or reroute
        if (r.destinationIP > 0 && r.destinationUUID.equals(getRouter().getID()))
        {
            r.output = getDirForIncomingItem(r);
            if (r.output == ForgeDirection.UNKNOWN)
            {
                r.resetTrip();
                LogisticPathFinder f = new LogisticPathFinder(getRouter(), r.payload.key()).setExclusions(r.travelLog).findBestResult();
                if (f.getResult() != null)
                {
                    r.setDestination(f.getResult().responder).setPriority(f.getResult().priority);
                    color = RouteFX.color_route;
                }
            }
            else
            {
                color = RouteFX.color_receive;
                r.hasArrived = true;
                itemArrived(r);
            }

            r.travelLog.set(getRouter().getIPAddress());
        }

        // Relay item
        if (r.output == ForgeDirection.UNKNOWN)
        {
            r.output = getRouter().getExitDirection(r.destinationIP);
            color = RouteFX.color_relay;
        }

        // Set to wander, clear travel log
        if (r.output == ForgeDirection.UNKNOWN)
        {
            super.resolveDestination(r);
            r.resetTrip();
            r.travelLog.clear();
            color = RouteFX.color_routeLost;
        }
        RouteFX.spawnType1(color, 8, new BlockCoord(tile()), world());
        adjustSpeed(r);
    }

    public ForgeDirection getDirForIncomingItem(RoutedPayload r)
    {
        return ForgeDirection.getOrientation(inOutSide);
    }

    @Override
    public void adjustSpeed(RoutedPayload r)
    {
        r.speed = r.priority.boost;
    }

    @Override
    public void trackedItemLost(ItemKeyStack s)
    {
    }

    @Override
    public void trackedItemReceived(ItemKeyStack s)
    {
    }

    @Override
    public int getActiveFreeSpace(ItemKey item)
    {
        IInventory real = getInventory();
        if (real == null)
            return 0;
        int side = getInterfacedSide();
        InventoryWrapper inv = InventoryWrapper.wrapInventory(real).setSlotsFromSide(side);
        int free = inv.getRoomAvailableForItem(item);
        return free;
    }

    @Override
    public IWorldRouter getWorldRouter()
    {
        return this;
    }

    @Override
    public IWorldBroadcaster getBroadcaster()
    {
        if (this instanceof IWorldBroadcaster)
            return (IWorldBroadcaster)this;

        return null;
    }

    @Override
    public IWorldRequester getRequester()
    {
        return this;
    }

    @Override
    public World getWorld()
    {
        return world();
    }

    @Override
    public BlockCoord getCoords()
    {
        return new BlockCoord(tile());
    }
}
