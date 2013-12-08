package mrtjp.projectred.transportation;

import java.util.BitSet;
import java.util.UUID;

import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.core.utils.ItemKeyStack;
import net.minecraft.entity.item.EntityItem;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraftforge.common.ForgeDirection;

public class RoutedPayload {

    ItemKeyStack payload;

    private static int maxID = 0;
    public final int payloadID;

    protected float speed = 0.01F;
    public double x, y, z;

    public ForgeDirection input = ForgeDirection.UNKNOWN;
    public ForgeDirection output = ForgeDirection.UNKNOWN;

    public boolean isEntering = true;

    public BasicPipePart parent;

    public RoutedPayload() {
        this(maxID < Short.MAX_VALUE ? ++maxID : (maxID = Short.MIN_VALUE));
    }
    public RoutedPayload(int id) {
        this.payloadID = id;
    }

    public RoutedPayload(double x, double y, double z, ItemKeyStack stack) {
        this();
        this.x = x;
        this.y = y;
        this.z = z;
        this.payload = stack;
    }

    public void bind(BasicPipePart p) {
        parent = p;
    }

    public void reset() {
        isEntering = true;
        input = ForgeDirection.UNKNOWN;
        output = ForgeDirection.UNKNOWN;
    }

    public void setPosition(double x, double y, double z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    public void movePosition(double x, double y, double z) {
        this.x += x;
        this.y += y;
        this.z += z;
    }

    public float getSpeed() {
        return speed;
    }

    public void setSpeed(float speed) {
        this.speed = speed;
    }

    public ItemStack getItemStack() {
        return payload.makeStack();
    }

    public void setItemStack(ItemStack item) {
        this.payload = ItemKeyStack.get(item);
    }

    public boolean isCorrupted() {
        return getItemStack() == null || getItemStack().stackSize <= 0 || Item.itemsList[getItemStack().itemID] == null;
    }

    @Override
    public int hashCode() {
        return payloadID;
    }

    @Override
    public boolean equals(Object o) {
        if (o instanceof RoutedPayload)
            return ((RoutedPayload)o).payloadID == payloadID;

        return false;
    }

    public void load(NBTTagCompound tag) {
        setPosition(tag.getDouble("x"), tag.getDouble("y"), tag.getDouble("z"));

        setSpeed(tag.getFloat("speed"));
        setItemStack(ItemStack.loadItemStackFromNBT(tag.getCompoundTag("Item")));

        isEntering = tag.getBoolean("isEnt");
        input = ForgeDirection.getOrientation(tag.getInteger("input"));
        output = ForgeDirection.getOrientation(tag.getInteger("output"));

        loadRouting(tag);
    }

    public void save(NBTTagCompound tag) {
        tag.setDouble("x", x);
        tag.setDouble("y", y);
        tag.setDouble("z", z);
        tag.setFloat("speed", getSpeed());
        NBTTagCompound nbttagcompound2 = new NBTTagCompound();
        getItemStack().writeToNBT(nbttagcompound2);
        tag.setCompoundTag("Item", nbttagcompound2);

        tag.setBoolean("isEnt", isEntering);
        tag.setInteger("input", input.ordinal());
        tag.setInteger("output", output.ordinal());

        saveRouting(tag);
    }


    public void move(double step) {
        ForgeDirection orientation = isEntering ? input : output;
        switch (orientation) {
        case UP: y += step; break;
        case DOWN: y -= step; break;
        case SOUTH: z += step; break;
        case NORTH: z -= step; break;
        case EAST: x += step; break;
        case WEST: x -= step; break;
        default:
        }
    }

    public EntityItem getEntityForDrop() {
        EntityItem item = new EntityItem(parent.world(), x, y+0.15, z, payload.makeStack());

        item.motionX = item.motionY = item.motionZ = item.hoverStart = 0;
        ForgeDirection orientation = isEntering ? input : output;
        switch (orientation) {
        case UP: item.motionY = +getSpeed(); break;
        case DOWN: item.motionY = -getSpeed(); break;
        case SOUTH: item.motionZ = +getSpeed(); break;
        case NORTH: item.motionZ = -getSpeed(); break;
        case EAST: item.motionX = +getSpeed(); break;
        case WEST: item.motionX = -getSpeed(); break;
        default:
        }
        item.delayBeforeCanPickup = 10;
        item.lifespan = 1600;
        return item;
    }

    /** Server-side Routing **/

    int destinationIP = -1;
    UUID destinationUUID = null;

    boolean hasArrived = false;

    BitSet travelLog = new BitSet();

    SendPriority priority = SendPriority.WANDERING;

    public static enum SendPriority {
        WANDERING("Wandering", 0.02f, 0.05f, PRColors.RED.ordinal()),
        DEFAULT("Default", 0.05f, 0.10f, PRColors.ORANGE.ordinal()),
        TERMINATED("Terminated", 0.02f, 0.05f, PRColors.PURPLE.ordinal()),
        PASSIVE("Passive", 0.10f, 0.20f, PRColors.BLUE.ordinal()),
        ACTIVE("Active", 0.20f, 0.30f, PRColors.GREEN.ordinal()),
        ;
        public final float speed;
        public final float boost;
        public final int color;
        public final String name;

        private SendPriority(String name, float speed, float boost, int color) {
            this.name = name;
            this.speed = speed;
            this.color = color;
            this.boost = boost;
        }
    }

    public RoutedPayload setDestination(int ip) {
        destinationIP = ip;
        Router router = RouterServices.instance.getRouter(ip);
        if (router != null)
            this.destinationUUID = router.getID();
        else
            destinationIP = -1;
        return this;
    }

    public RoutedPayload setPriority(SendPriority priority) {
        this.priority = priority;
        return this;
    }

    public RoutedPayload resetTrip() {
        if (destinationIP > -1) {
            Router r = RouterServices.instance.getRouter(destinationIP);
            if (r != null) {
                IWorldRouter parent = r.getParent();
                if (parent instanceof IWorldRequester)
                    ((IWorldRequester) parent).trackedItemLost(payload);
            }
        }
        destinationIP = -1;
        destinationUUID = null;
        hasArrived = false;
        priority = SendPriority.WANDERING;
        return this;
    }

    public void refreshIP() {
        Router router = RouterServices.instance.getRouter(destinationIP);
        if (router == null || router.getID() != destinationUUID)
            destinationIP = RouterServices.instance.getIPforUUID(destinationUUID);
    }

    public void saveRouting(NBTTagCompound tag) {
        //TODO maybe work on a way to save/load routing
    }

    public void loadRouting(NBTTagCompound tag) {
        //TODO maybe work on a way to save/load routing
    }

}
