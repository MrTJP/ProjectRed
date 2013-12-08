package mrtjp.projectred.transportation;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.inventory.InventoryWrapper;
import mrtjp.projectred.transportation.RoutedPayload.SendPriority;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.inventory.IInventory;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.util.Icon;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.lighting.LazyLightMatrix;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.IconTransformation;
import codechicken.lib.render.RenderUtils;
import codechicken.lib.render.TextureUtils;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Translation;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.TMultiPart;

import com.google.common.collect.BiMap;
import com.google.common.collect.ForwardingSet;
import com.google.common.collect.HashBiMap;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class BasicPipePart extends CorePipePart
{
    protected PayloadMovement itemFlow = new PayloadMovement();
    public PipeLogic logic;
    public boolean initialized = false;
    
    private List<IInventory> cachedInventories;

    @Override
    public void preparePlacement(int meta) {
        super.preparePlacement(meta);
        logic = PipeLogic.createPipeLogic(this, meta);
    }

    protected static class PayloadMovement extends ForwardingSet<RoutedPayload>
    {
        private final BiMap<Integer, RoutedPayload> delegate = HashBiMap.create();
        private final Set<RoutedPayload> inputQueue = new HashSet<RoutedPayload>();
        private final Set<RoutedPayload> outputQueue = new HashSet<RoutedPayload>();
        private int delay = 0;

        @Override
        public Set<RoutedPayload> delegate() {
            return delegate.values();
        }

        @Override
        public boolean add(RoutedPayload item) {
            if (delegate.containsValue(item))
                return false;

            delegate.put(item.payloadID, item);
            return true;
        }

        @Override
        public boolean addAll(Collection<? extends RoutedPayload> collection) {
            boolean changed = false;
            for (RoutedPayload item : collection)
                changed |= add(item);

            return changed;
        }

        public RoutedPayload get(int id) {
            return delegate.get(id);
        }

        public void scheduleLoad(RoutedPayload item) {
            delay = 10;
            inputQueue.add(item);
        }

        public void executeLoad() {
            if (delay-- > 0)
                return;

            addAll(inputQueue);
            inputQueue.clear();
        }

        public boolean scheduleRemoval(RoutedPayload item) {
            return outputQueue.add(item);
        }

        public boolean unscheduleRemoval(RoutedPayload item) {
            return outputQueue.remove(item);
        }

        public void exececuteRemove() {
            removeAll(outputQueue);
            outputQueue.clear();
        }
    };

    public PipeLogic getLogic() {
        return logic;
    }

    @Override
    public void update() {
        if (!initialized)
            initialized = true;

        pushItemFlow();
        getLogic().tick();
    }

    protected void pushItemFlow() {
        itemFlow.executeLoad();
        itemFlow.exececuteRemove();

        for (RoutedPayload r : itemFlow) {
            if (r.isCorrupted()) {
                itemFlow.scheduleRemoval(r);
                continue;
            }

            r.move(r.getSpeed());

            if (r.isEntering && hasReachedMiddle(r) || hasInvalidLoc(r)) {
                r.isEntering = false;
                r.setPosition(x() + 0.5D, y() + 0.25D, z() + 0.5D);

                if (r.output == ForgeDirection.UNKNOWN)
                    handleDrop(r);
                else
                    centerReached(r);

            } else if (!r.isEntering && hasReachedEnd(r) && itemFlow.scheduleRemoval(r))
                endReached(r);
        }

        itemFlow.exececuteRemove();
    }

    public void handleDrop(RoutedPayload r) {
        if (getLogic().handleDrop(r)) return;

        if (itemFlow.scheduleRemoval(r))
            if (!world().isRemote) {
                r.resetTrip();
                world().spawnEntityInWorld(r.getEntityForDrop());
            }
    }

    public void resolveDestination(RoutedPayload r) {
        if (getLogic().resolveDestination(r)) return;

        chooseRandomDestination(r);
    }

    public void chooseRandomDestination(RoutedPayload r) {
        LinkedList<ForgeDirection> movements = new LinkedList<ForgeDirection>();

        for (int i = 0; i < 6; i++) {
            if ((connMap & 1<<i) == 0) continue;
            if (i == r.input.getOpposite().ordinal()) continue;
            BlockCoord bc = new BlockCoord(tile()).offset(i);
            TMultiPart t = BasicUtils.getMultiPart(world(), bc, 6);
            if (t instanceof BasicPipePart)
                movements.add(ForgeDirection.getOrientation(i));
        }

        if (movements.isEmpty())
            r.output = r.input.getOpposite();
        else
            r.output = movements.get(world().rand.nextInt(movements.size()));
    }

    public void endReached(RoutedPayload r) {
        if (getLogic().centerReached(r)) return;
        if (!world().isRemote)
            if (!maskConnects(r.output.ordinal()) || !passToNextPipe(r)) {
                // Injection to inventories
                IInventory inv = InventoryWrapper.getInventory(world(), new BlockCoord(tile()).offset(r.output.ordinal()));
                if (inv != null) {
                    InventoryWrapper w = InventoryWrapper.wrapInventory(inv).setSlotsFromSide(r.output.getOpposite().ordinal());
                    r.payload.stackSize -= w.injectItem(r.payload.makeStack(), true);
                }
                // Bounce
                if (r.payload.stackSize > 0)
                    bounceStack(r);
            }
    }

    public void bounceStack(RoutedPayload r) {
        itemFlow.unscheduleRemoval(r);
        r.isEntering = true;
        r.input = r.output.getOpposite();
        resolveDestination(r);
        adjustSpeed(r);
        adjustLoc(r);
        sendItemUpdate(r);
    }

    public void centerReached(RoutedPayload r) {
        if (getLogic().centerReached(r)) return;

        if (!maskConnects(r.output.ordinal()))
            resolveDestination(r);
    }

    public boolean passToNextPipe(RoutedPayload r) {
        TMultiPart p = BasicUtils.getMultiPart(world(), new BlockCoord(tile()).offset(r.output.ordinal()), 6);
        if (p instanceof BasicPipePart) {
            BasicPipePart pipe = (BasicPipePart) p;
            pipe.injectPayload(r, r.output);
            return true;
        }
        return false;
    }

    public void adjustSpeed(RoutedPayload r) {
        r.setSpeed(Math.max(r.getSpeed()-0.01f, r.priority.speed));
    }

    private void adjustLoc(RoutedPayload r) {
        double x = r.x;
        double y = r.y;
        double z = r.z;

        x = Math.max(x, x() + 0.01);
        y = Math.max(y, y() + 0.01);
        z = Math.max(z, z() + 0.01);

        x = Math.min(x, x() + 0.99);
        y = Math.min(y, y() + 0.99);
        z = Math.min(z, z() + 0.99);

        if (r.input != ForgeDirection.UP && r.input != ForgeDirection.DOWN)
            y = y() + 0.25F;

        r.setPosition(x, y, z);
    }

    protected boolean hasReachedMiddle(RoutedPayload r) {
        float middleLimit = r.getSpeed() * 1.01F;
        return Math.abs(x() + 0.5 - r.x) < middleLimit
                && Math.abs(y() + 0.25f - r.y) < middleLimit
                && Math.abs(z() + 0.5 - r.z) < middleLimit;
    }

    protected boolean hasReachedEnd(RoutedPayload r) {
        return r.x > x() + 1
                || r.x < x()
                || r.y > y() + 1
                || r.y < y()
                || r.z > z() + 1
                || r.z < z();
    }

    protected boolean hasInvalidLoc(RoutedPayload r) {
        return r.x > x() + 2
                || r.x < x() - 1
                || r.y > y() + 2
                || r.y < y() - 1
                || r.z > z() + 2
                || r.z < z() - 1;
    }

    public void injectPayload(RoutedPayload r, ForgeDirection in) {
        if (r.isCorrupted())
            return;

        r.bind(this);
        r.reset();
        r.input = in;
        itemFlow.add(r);

        adjustSpeed(r);
        adjustLoc(r);
        if (!world().isRemote) {
            resolveDestination(r);
            sendItemUpdate(r);
        }
    }

    @Override
    public void save(NBTTagCompound tag) {
        super.save(tag);
        NBTTagList nbttaglist = new NBTTagList();

        for (RoutedPayload r : itemFlow) {
            NBTTagCompound payloadData = new NBTTagCompound();
            nbttaglist.appendTag(payloadData);
            r.save(payloadData);
        }

        tag.setTag("itemFlow", nbttaglist);

        getLogic().save(tag);
    }

    @Override
    public void load(NBTTagCompound tag) {
        super.load(tag);
        NBTTagList nbttaglist = tag.getTagList("itemFlow");

        for (int j = 0; j < nbttaglist.tagCount(); ++j)
            try {
                NBTTagCompound payloadData = (NBTTagCompound) nbttaglist.tagAt(j);

                RoutedPayload r = new RoutedPayload();
                r.bind(this);
                r.load(payloadData);

                if (r.isCorrupted())
                    continue;

                itemFlow.scheduleLoad(r);
            } catch (Throwable t) {}

        logic = PipeLogic.createPipeLogic(this, meta);
        getLogic().load(tag);
    }

    @Override
    public void writeDesc(MCDataOutput packet) {
        super.writeDesc(packet);

        getLogic().writeDesc(packet);
    }

    @Override
    public void readDesc(MCDataInput packet) {
        super.readDesc(packet);

        if (getLogic() == null)
            logic = PipeLogic.createPipeLogic(this, meta);
        getLogic().readDesc(packet);
    }

    @Override
    public void read(MCDataInput packet, int switch_key) {
        super.read(packet, switch_key);

        if (switch_key == 1)
            handleItemUpdatePacket(packet);

        getLogic().read(packet, switch_key);
    }
    
    @Override
    public void onPartChanged(TMultiPart part) {
        super.onPartChanged(part);
        cachedInventories = null;
    }

    @Override
    public void onNeighborChanged() {
        super.onNeighborChanged();
        cachedInventories = null;

        int connCount = 0;
        for (int i = 0; i < 6; i++)
            if ((connMap & 1<<i) != 0)
                connCount++;
        if (connCount == 0)
            for (RoutedPayload r : itemFlow)
                if (itemFlow.scheduleRemoval(r))
                    if (!world().isRemote) {
                        r.resetTrip();
                        world().spawnEntityInWorld(r.getEntityForDrop());
                    }
    }

    @Override
    public void onRemoved() {
        super.onRemoved();
        if (!world().isRemote)
            for (RoutedPayload r : itemFlow) {
                r.resetTrip();
                world().spawnEntityInWorld(r.getEntityForDrop());
            }
    }

    public void sendItemUpdate(RoutedPayload r) {
        MCDataOutput out = tile().getWriteStream(this).writeByte(1);

        out.writeShort(r.payloadID);

        out.writeFloat((float) r.x);
        out.writeFloat((float) r.y);
        out.writeFloat((float) r.z);

        out.writeItemStack(r.getItemStack());

        out.writeByte((byte) r.input.ordinal());
        out.writeByte((byte) r.output.ordinal());

        out.writeFloat(r.getSpeed());

        out.writeByte(r.priority.ordinal());
    }

    public void handleItemUpdatePacket(MCDataInput packet) {
        int id = packet.readShort();

        RoutedPayload r = itemFlow.get(id);
        if (r == null) {
            r = new RoutedPayload(id);
            r.setPosition(packet.readFloat(), packet.readFloat(), packet.readFloat());
            itemFlow.add(r);
        } else
            for (int i = 0; i < 3; i++)
                packet.readFloat();

        r.setItemStack(packet.readItemStack());
        r.input = ForgeDirection.getOrientation(packet.readByte());
        r.output = ForgeDirection.getOrientation(packet.readByte());
        r.setSpeed(packet.readFloat());
        r.setPriority(SendPriority.values()[packet.readByte()]);
    }

    @Override
    public String getType() {
        return "pr_ptube";
    }

    @Override
    @SideOnly(Side.CLIENT)
    public void drawBreaking(RenderBlocks r) {
        for (Cuboid6 box : getCollisionBoxes())
            RenderUtils.renderBlock(box, 0, new Translation(x(), y(), z()), new IconTransformation(r.overrideBlockTexture), null);
    }

    @Override
    @SideOnly(Side.CLIENT)
    public void renderStatic(Vector3 pos, LazyLightMatrix olm, int pass) {
        if (pass == 0) {
            TextureUtils.bindAtlas(0);
            CCRenderState.reset();
            CCRenderState.setBrightness(world(), x(), y(), z());
            CCRenderState.useModelColours(true);

            RenderPipe.render(this, pos, olm);

            CCRenderState.setColour(-1);
        }
    }

    @Override
    @SideOnly(Side.CLIENT)
    public void renderDynamic(Vector3 pos, float frame, int pass) {
        if (pass == 0) {
            TextureUtils.bindAtlas(0);
            CCRenderState.reset();
            CCRenderState.setBrightness(world(), x(), y(), z());
            CCRenderState.useModelColours(true);

            RenderPipe.renderItemFlow(this, pos, frame);
            CCRenderState.setColour(-1);
        }
    }

    /**
     * 0 to 5 for sides, 6 for center
     */
    public Icon getIcon(int side) {
        return getLogic().getIcon(this, side);
    }

    public List<IInventory> getConnectedInventories() {
        if (cachedInventories != null)
            return cachedInventories;
        
        List<IInventory> adjacent = new ArrayList<IInventory>();
        BlockCoord bc = new BlockCoord(tile());

        for (int i = 0; i < 6; i++)
            if (maskConnects(i)) {
                IInventory inv = InventoryWrapper.getInventory(world(), bc.copy().offset(i));
                if (inv != null)
                    adjacent.add(inv);
            }
        
        cachedInventories = Collections.unmodifiableList(adjacent);
        return cachedInventories;
    }
}
