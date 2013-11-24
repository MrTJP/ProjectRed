package mrtjp.projectred.expansion;

import java.util.BitSet;
import java.util.LinkedList;
import java.util.UUID;

import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.core.utils.ItemKeyStack;
import mrtjp.projectred.expansion.RoutedPayload.SendPriority;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.Icon;
import net.minecraft.util.MovingObjectPosition;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.vec.BlockCoord;

public class RoutedJunctionPipePart extends BasicPipePart implements IWorldRouter, IRouteLayer, IWorldRequester {

    public int linkMap;

    public Router router;
    public String routerID;
    public Object routerIDLock = new Object();

    public boolean needsWork = true;
    public boolean firstTick = true;

    public static int pipes = 0;
    public int searchDelay = 0;

    private LinkedList<RoutedPayload> sendQueue = new LinkedList<RoutedPayload>();

    public RoutedJunctionPipePart() {
        pipes++;
        searchDelay = pipes % Configurator.detectionFrequency;
    }

    @Override
    public Router getRouter() {
        if (needsWork)
            return null;

        if (router == null)
            synchronized (routerIDLock) {
                UUID id = null;
                if (routerID != null && !routerID.isEmpty())
                    id = UUID.fromString(routerID);
                router = RouterServices.instance.getOrCreateRouter(id, world().provider.dimensionId, new BlockCoord(tile()));
            }

        return router;
    }

    @Override
    public void itemEnroute(RoutedPayload r) {
    }

    @Override
    public void itemArrived(RoutedPayload r) {
        if (this instanceof IWorldRequester)
            ((IWorldRequester) this).trackedItemReceived(r.payload);
    }

    @Override
    public void queueStackToSend(ItemStack stack, int dirToInventory, SyncResponse path) {
        queueStackToSend(stack, dirToInventory, path.priority, path.responder);
    }

    @Override
    public void queueStackToSend(ItemStack stack, int dirToInventory, SendPriority priority, int destination) {
        RoutedPayload r = new RoutedPayload(x() + 0.5, y() + 0.25, z() + 0.5, ItemKeyStack.get(stack));
        r.input = ForgeDirection.getOrientation(dirToInventory);
        r.setPriority(priority);
        r.setDestination(destination);
        sendQueue.addLast(r);
    }

    private void dispatchQueuedPayload(RoutedPayload r) {
        double x = r.x;
        double y = r.y;
        double z = r.z;
        double step = 0.5;
        switch (r.input.getOpposite()) {
        case UP: y += step; break;
        case DOWN: y -= step; break;
        case SOUTH: z += step; break;
        case NORTH: z -= step; break;
        case EAST: x += step; break;
        case WEST: x -= step; break;
        default:
        }

        r.setPosition(x, y, z);

        injectPayload(r, r.input);
        Router dest = RouterServices.instance.getRouter(r.destinationIP);
        if (dest != null) {
            IWorldRouter wr = dest.getParent();
            wr.itemEnroute(r);
        }
    }

    @Override
    public SyncResponse getLogisticPath(ItemKey stack, BitSet exclusions, boolean excludeStart) {
        LogisticPathFinder p = new LogisticPathFinder(getRouter(), stack);
        if (exclusions != null)
            p.setExclusions(exclusions);
        p.setExcludeSource(excludeStart).findBestResult();
        return p.getResult();
    }

    @Override
    public SyncResponse getSyncResponse(ItemKey item, SyncResponse rival) {
        return null;
    }


    @Override
    public void update() {
        if (needsWork) {
            needsWork = false;
            coldBoot();
            return;
        }
        if (!world().isRemote)
            getRouter().update(world().getTotalWorldTime() % Configurator.detectionFrequency == searchDelay || firstTick);

        super.update();
        firstTick = false;

        // Dispatch queued items
        if (!sendQueue.isEmpty())
            dispatchQueuedPayload(sendQueue.removeFirst());
    }

    public void coldBoot() {
        if (!world().isRemote)
            getRouter();
    }

    @Override
    public boolean needsWork() {
        return needsWork;
    }

    @Override
    public boolean refreshState() {
        if (world().isRemote) return false;
        int link = 0;
        for (ForgeDirection d : ForgeDirection.VALID_DIRECTIONS)
            if (getRouter().LSAExists(d))
                link |= 1<<d.ordinal();
        if (linkMap != link) {
            linkMap = link;
            sendLinkMapUpdate();
            return true;
        }
        return false;
    }

    @Override
    public BasicPipePart getContainer() {
        return this;
    }

    @Override
    public boolean activate(EntityPlayer player, MovingObjectPosition hit, ItemStack item) {
        return false;
    }

    @Override
    public void read(MCDataInput packet, int switch_key) {
        if (switch_key == 51) {
            linkMap = packet.readUByte();
            tile().markRender();
        } else
            super.read(packet, switch_key);
    }

    public void sendLinkMapUpdate() {
        tile().getWriteStream(this).writeByte(51).writeByte(linkMap);
    }

    @Override
    public Icon getIcon(int side) {
        if (side == 6)
            return super.getIcon(side);
        if ((linkMap&1<<side) != 0)
            return EnumPipe.ROUTEDJUNCTION.sprites[0];
        else
            return EnumPipe.ROUTEDJUNCTION.sprites[1];
    }

    @Override
    public String getType() {
        return "pr_rbasic";
    }

    @Override
    public void onRemoved() {
        super.onRemoved();
        pipes = Math.max(pipes-1, 0);
        if (getRouter() != null)
            getRouter().decommission();
    }

    @Override
    public void save(NBTTagCompound tag) {
        super.save(tag);
        synchronized (routerIDLock) {
            if (routerID == null || routerID.isEmpty())
                if(router != null)
                    routerID = getRouter().getID().toString();
                else
                    routerID = UUID.randomUUID().toString();
        }
        tag.setString("rid", routerID);
    }

    @Override
    public void load(NBTTagCompound tag) {
        super.load(tag);
        synchronized (routerIDLock) {
            routerID = tag.getString("rid");
        }
    }

    @Override
    public void writeDesc(MCDataOutput packet) {
        super.writeDesc(packet);
        packet.writeByte(linkMap);
    }

    @Override
    public void readDesc(MCDataInput packet) {
        super.readDesc(packet);
        linkMap = packet.readByte();
    }

    @Override
    public void resolveDestination(RoutedPayload r) {
        if (needsWork())
            return;

        r.output = ForgeDirection.UNKNOWN;

        // Reroute item if it needs one
        r.refreshIP();
        if (r.destinationIP < 0 || r.destinationIP >= 0 && r.hasArrived) {
            r.resetTrip();
            LogisticPathFinder f = new LogisticPathFinder(getRouter(), r.payload.key()).setExclusions(r.travelLog).findBestResult();
            if (f.getResult() != null)
                r.setDestination(f.getResult().responder).setPriority(f.getResult().priority);
        }
        r.refreshIP();

        // Deliver item, or reroute
        if (r.destinationIP > 0 && r.destinationUUID.equals(getRouter().getID())) {
            r.output = getDirForIncomingItem(r);
            if (r.output == ForgeDirection.UNKNOWN) {
                r.resetTrip();
                LogisticPathFinder f = new LogisticPathFinder(getRouter(), r.payload.key()).setExclusions(r.travelLog).findBestResult();
                if (f.getResult() != null)
                    r.setDestination(f.getResult().responder).setPriority(f.getResult().priority);
            }
            r.hasArrived = true;
            itemArrived(r);
            r.travelLog.set(getRouter().getIPAddress());
        }

        // Relay item
        if (r.output == ForgeDirection.UNKNOWN)
            r.output = getRouter().getExitDirection(r.destinationIP);

        // Set to wander, clear travel log
        if (r.output == ForgeDirection.UNKNOWN) {
            super.resolveDestination(r);
            r.resetTrip();
            r.travelLog.clear();
        }

        adjustSpeed(r);
    }

    public ForgeDirection getDirForIncomingItem(RoutedPayload r) {
        return ForgeDirection.UNKNOWN;
    }

    @Override
    public void adjustSpeed(RoutedPayload r) {
        r.setSpeed(r.priority.boost);
    }

    @Override
    public void trackedItemLost(ItemKeyStack s) {
    }

    @Override
    public void trackedItemReceived(ItemKeyStack s) {
    }

    @Override
    public IWorldRouter getWorldRouter() {
        return this;
    }

    @Override
    public IWorldBroadcaster getBroadcaster() {
        if (this instanceof IWorldBroadcaster)
            return (IWorldBroadcaster)this;

        return null;
    }

    @Override
    public IWorldRequester getRequester() {
        return this;
    }
}
