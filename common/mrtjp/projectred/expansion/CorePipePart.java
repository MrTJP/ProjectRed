package mrtjp.projectred.expansion;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.inventory.InventoryWrapper;
import mrtjp.projectred.expansion.RoutedPayload.SendPriority;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.IInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.Icon;
import net.minecraft.util.MovingObjectPosition;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.lighting.LazyLightMatrix;
import codechicken.lib.raytracer.IndexedCuboid6;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.IconTransformation;
import codechicken.lib.render.RenderUtils;
import codechicken.lib.render.TextureUtils;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Translation;
import codechicken.lib.vec.Vector3;
import codechicken.microblock.IHollowConnect;
import codechicken.multipart.JNormalOcclusion;
import codechicken.multipart.NormalOcclusionTest;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TSlottedPart;
import codechicken.multipart.TileMultipart;

import com.google.common.collect.BiMap;
import com.google.common.collect.ForwardingSet;
import com.google.common.collect.HashBiMap;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public abstract class CorePipePart extends TMultiPart implements IPipeConnectable, TSlottedPart, JNormalOcclusion, IHollowConnect
{
    public static Cuboid6[] boundingBoxes = new Cuboid6[7];
    private static int expandBounds = -1;
    static {
        double w = 2 / 8D;
        boundingBoxes[6] = new Cuboid6(0.5 - w, 0.5 - w, 0.5 - w, 0.5 + w, 0.5 + w, 0.5 + w);
        for (int s = 0; s < 6; s++)
            boundingBoxes[s] = new Cuboid6(0.5 - w, 0, 0.5 - w, 0.5 + w, 0.5 - w, 0.5 + w).apply(Rotation.sideRotations[s].at(Vector3.center));
    }

    /**
     * 6 bits, flagged for an external connection to that side.
     */
    public int connMap;
    public byte meta;

    public void preparePlacement(int meta) {
        this.meta = (byte) meta;
    }

    @Override
    public int getHollowSize() {
        return 8;
    }

    @Override
    public float getStrength(MovingObjectPosition hit, EntityPlayer player) {
        return 2;
    }

    @Override
    public boolean occlusionTest(TMultiPart npart) {
        return NormalOcclusionTest.apply(this, npart);
    }

    @Override
    public Iterable<Cuboid6> getOcclusionBoxes() {
        if (expandBounds >= 0)
            return Arrays.asList(boundingBoxes[expandBounds]);

        return Arrays.asList(boundingBoxes[6]);
    }

    @Override
    public Iterable<IndexedCuboid6> getSubParts() {
        Iterable<Cuboid6> boxList = getCollisionBoxes();
        LinkedList<IndexedCuboid6> partList = new LinkedList<IndexedCuboid6>();
        for (Cuboid6 c : boxList)
            partList.add(new IndexedCuboid6(0, c));
        return partList;
    }

    @Override
    public Iterable<Cuboid6> getCollisionBoxes() {
        LinkedList<Cuboid6> list = new LinkedList<Cuboid6>();
        list.add(boundingBoxes[6]);
        for (int s = 0; s < 6; s++)
            if (maskConnects(s))
                list.add(boundingBoxes[s]);
        return list;
    }

    @Override
    public int getSlotMask() {
        return 0x40;
    }

    @Override
    public void save(NBTTagCompound tag) {
        super.save(tag);
        tag.setInteger("connMap", connMap);
        tag.setByte("meta", meta);
    }

    @Override
    public void load(NBTTagCompound tag) {
        super.load(tag);
        connMap = tag.getInteger("connMap");
        meta = tag.getByte("meta");
    }

    @Override
    public void writeDesc(MCDataOutput packet) {
        packet.writeByte(clientConnMap());
        packet.writeByte(meta);
    }

    @Override
    public void readDesc(MCDataInput packet) {
        connMap = packet.readUByte();
        meta = packet.readByte();
    }
    
    @Override
    public void read(MCDataInput packet) {
        read(packet, packet.readUByte());
    }

    public void read(MCDataInput packet, int switch_key) {
        if (switch_key == 0) {
            connMap = packet.readUByte();
            tile().markRender();
        }
    }

    @Override
    public void onNeighborChanged() {
        if (!world().isRemote)
            if (updateExternalConnections())
                sendConnUpdate();
    }

    @Override
    public void onPartChanged(TMultiPart part) {
        if (!world().isRemote) {
            boolean changed = false;
            if (updateOpenConnections())
                changed |= updateExternalConnections();
            if (changed)
                sendConnUpdate();
        }
    }

    @Override
    public void onAdded() {
        if (!world().isRemote) {
            updateOpenConnections();
            if (updateExternalConnections())
                sendConnUpdate();
        }
    }

    @Override
    public void onRemoved() {
        super.onRemoved();
        if (!world().isRemote) {
            for (int s = 0; s < 6; s++)
                if ((connMap & 1 << s) != 0)
                    notifyStraightChange(s);
        }
    }

    @Override
    public Iterable<ItemStack> getDrops() {
        return Arrays.asList(getItem());
    }

    @Override
    public ItemStack pickItem(MovingObjectPosition hit) {
        return getItem();
    }

    public ItemStack getItem() {
        return EnumPipe.VALID_PIPE[meta].getItemStack();
    }

    public void notifyStraightChange(int s) {
        BlockCoord pos = new BlockCoord(tile()).offset(s);
        world().notifyBlockOfNeighborChange(pos.x, pos.y, pos.z, tile().getBlockType().blockID);
    }

    @Override
    public void onChunkLoad() {}

    @Override
    public void onWorldJoin() {
        onNeighborChanged();
    }

    public int clientConnMap() {
        return connMap & 0x3F | connMap >> 6 & 0x3F;
    }

    public void sendConnUpdate() {
        tile().getWriteStream(this).writeByte(0).writeByte(clientConnMap());
    }

    /**
     * Recalculates connections to blocks outside this sapce
     * 
     * @return true if a new connection was added or one was removed
     */
    protected boolean updateExternalConnections() {
        int newConn = 0;
        for (int s = 0; s < 6; s++) {
            if (!maskOpen(s))
                continue;

            if (connect(s))
                newConn |= 1 << s;
        }

        if (newConn != (connMap & 0x3F)) {
            connMap = connMap & ~0x3F | newConn;
            return true;
        }
        return false;
    }

    /**
     * Recalculates connections that can be made to other parts outside of this
     * space
     * 
     * @return true if external connections should be recalculated
     */
    protected boolean updateOpenConnections() {
        int newConn = 0;
        for (int s = 0; s < 6; s++)
            if (connectionOpen(s))
                newConn |= 1 << s + 12;

        if (newConn != (connMap & 0x3F000)) {
            connMap = connMap & ~0x3F000 | newConn;
            return true;
        }
        return false;
    }

    public boolean connectionOpen(int s) {
        TMultiPart facePart = tile().partMap(s);
        if (facePart == null)
            return true;

        if (facePart instanceof IPipeConnectable && canConnectTo((IPipeConnectable) facePart))
            return false;

        expandBounds = s;
        boolean fits = tile().canReplacePart(this, this);
        expandBounds = -1;

        return fits;
    }

    public boolean connect(int absDir) {
        BlockCoord pos = new BlockCoord(tile()).offset(absDir);
        TileEntity te = BasicUtils.getTileEntity(world(), pos, TileEntity.class);
        if (te instanceof TileMultipart) {
            TileMultipart t = (TileMultipart)te;
            TMultiPart tp = t.partMap(6);
            if (tp instanceof IPipeConnectable)
                return ((IPipeConnectable) tp).connect(this, absDir ^ 1);
        }

        return false;
    }

    @Override
    public boolean connect(IPipeConnectable tube, int fromAbsDir) {
        if (canConnectTo(tube) && maskOpen(fromAbsDir)) {
            int oldConn = connMap;
            connMap |= 1 << fromAbsDir;
            if (oldConn != connMap)
                sendConnUpdate();
            return true;
        }
        return false;
    }

    @Override
    public boolean canConnectTo(IPipeConnectable tube) {
        return true;
    }

    @Override
    public abstract String getType();

    public boolean maskConnects(int absDir) {
        return (connMap & 0x41 << absDir) != 0;
    }

    public boolean maskOpen(int absDir) {
        return (connMap & 0x1000 << absDir) != 0;
    }
}
