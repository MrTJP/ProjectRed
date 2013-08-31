package mrtjp.projectred.expansion;

import java.util.Arrays;
import java.util.LinkedList;

import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.transmission.EnumWire;
import mrtjp.projectred.transmission.RenderFramedWire;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.MovingObjectPosition;
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
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class PressurizedTubePart extends TMultiPart implements ITubeInterface, TSlottedPart, JNormalOcclusion, IHollowConnect {

    public static Cuboid6[] boundingBoxes = new Cuboid6[7];
    private static int expandBounds = -1;

    static {
        double w = 2 / 8D;
        boundingBoxes[6] = new Cuboid6(0.5 - w, 0.5 - w, 0.5 - w, 0.5 + w, 0.5 + w, 0.5 + w);
        for (int s = 0; s < 6; s++)
            boundingBoxes[s] = new Cuboid6(0.5 - w, 0, 0.5 - w, 0.5 + w, 0.5 - w, 0.5 + w).apply(Rotation.sideRotations[s].at(Vector3.center));
    }

    /**
     * lowest 6 bits, flagged for an external connection to that side next 6
     * bits, flagged for an internal connection to that side next 6 bits,
     * flagged for an open connection to that side
     * 
     * On the client, only the lowest 6 bits contain actual connections.
     */
    public int connMap;

    public LinkedList<TubeItem> itemFlow = new LinkedList<TubeItem>();

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
    }

    @Override
    public void load(NBTTagCompound tag) {
        super.load(tag);
        connMap = tag.getInteger("connMap");
    }

    @Override
    public void writeDesc(MCDataOutput packet) {
        packet.writeByte(clientConnMap());
    }

    @Override
    public void readDesc(MCDataInput packet) {
        connMap = packet.readUByte();
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
        if (!world().isRemote) {
            if (updateExternalConnections())
                sendConnUpdate();
        }
    }

    @Override
    public void onPartChanged(TMultiPart part) {
        if (!world().isRemote) {
            boolean changed = false;
            if (updateOpenConnections())
                changed |= updateExternalConnections();
            if (changed) {
                sendConnUpdate();
            }
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

    public void notifyStraightChange(int s) {
        BlockCoord pos = new BlockCoord(getTile()).offset(s);
        world().notifyBlockOfNeighborChange(pos.x, pos.y, pos.z, getTile().getBlockType().blockID);
    }

    @Override
    public void onChunkLoad()// do nothing on chunk load, we shouldn't have
                             // changed between saves
    {
    }

    @Override
    public void onWorldJoin()// when we're moved by a frame or something
    {
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
            connMap = (connMap & ~0x3F) | newConn;
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
                newConn |= 1 << (s + 12);

        if (newConn != (connMap & 0x3F000)) {
            connMap = (connMap & ~0x3F000) | newConn;
            return true;
        }
        return false;
    }

    public boolean connectionOpen(int s) {
        TMultiPart facePart = tile().partMap(s);
        if (facePart == null)
            return true;

        if (facePart instanceof ITubeConnectable && canConnectTo((ITubeConnectable) facePart))
            return false;

        expandBounds = s;
        boolean fits = tile().canReplacePart(this, this);
        expandBounds = -1;

        return fits;
    }

    public boolean connect(int absDir) {
        BlockCoord pos = new BlockCoord(getTile()).offset(absDir);
        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if (t != null) {
            TMultiPart tp = t.partMap(6);
            if (tp instanceof ITubeConnectable)
                return ((ITubeConnectable) tp).connect(this, absDir ^ 1);
        }
        return false;
    }

    @Override
    public boolean connect(ITubeConnectable tube, int fromAbsDir) {
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
    public String getType() {
        return "pr_ptube";
    }

    @Override
    public int getGCost() {
        return 2;
    }

    public boolean maskConnects(int absDir) {
        return (connMap & 0x41 << absDir) != 0;
    }

    public boolean maskOpen(int absDir) {
        return (connMap & 0x1000 << absDir) != 0;
    }

    @Override
    public boolean canAcceptItem(TubeItem item, int fromAbsDir) {
        return false;
    }

    @Override
    public boolean isDestinationForItem(TubeItem item, int fromAbsDir) {
        return false;
    }

    @Override
    public boolean canConnectTo(ITubeConnectable tube) {
        return true;
    }

    @Override
    public boolean addItem(TubeItem item, int fromAbsDir) {
        item.progress -= 100;
        item.hasCrossedCenter = false;
        item.pathFoundByParent = false;
        item.direction = (byte) (fromAbsDir ^ 1);
        return itemFlow.add(item);
    }

    public boolean addItemStack(ItemStack item, int fromAbsDir) {
        TubeItem t = new TubeItem(item, fromAbsDir);
        return addItem(t, fromAbsDir);
    }

    @Override
    public void update() {
        for (TubeItem t : itemFlow) {
            t.update();
            if (t.hasCrossedCenter) {
                if (!t.pathFoundByParent) {
                    // TODO Pathfind from here under these conditions:
                    // 1) If this pipe is a node (contains a turn point)
                    // 2) If item cannot continue straight.
                }
                t.pathFoundByParent = true;
            }
        }
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

            /** temporary render start **/
            int key = RenderFramedWire.modelKey(0, connMap);
            Translation t = new Translation(x(), y(), z());
            IconTransformation uvt = new IconTransformation(EnumWire.RED_ALLOY.wireSprites[0]);
            RenderFramedWire.frameModels[6].render(t, uvt);
            for (int s = 0; s < 6; s++)
                if ((key & 1 << s) != 0)
                    RenderFramedWire.frameModels[s].render(t, uvt);
            /** temporary render end **/

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
            // TODO add Tube Item rendering here
            CCRenderState.setColour(-1);
        }
    }

}
