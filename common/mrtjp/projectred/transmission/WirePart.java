package mrtjp.projectred.transmission;

import java.util.Arrays;

import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.CommandDebug;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.Icon;
import net.minecraft.util.MovingObjectPosition;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.lighting.LazyLightMatrix;
import codechicken.lib.raytracer.IndexedCuboid6;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.IRedstonePart;
import codechicken.multipart.JNormalOcclusion;
import codechicken.multipart.NormalOcclusionTest;
import codechicken.multipart.PartMap;
import codechicken.multipart.TFacePart;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;
import codechicken.multipart.scalatraits.TRedstoneTile;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

/**
 * This is the base class for all wire types. It can be used for any sub type,
 * as it contains the base calculations necessary to create a working wire. This
 * calculates all possible connections to sides, around corners, and inside
 * corners, while checking for microblock obstructions.
 * 
 * @author MrTJP
 * 
 */
public abstract class WirePart extends TMultiPart implements IConnectable, TFacePart, JNormalOcclusion {

    public static Cuboid6[][] selectionBounds = new Cuboid6[3][6];
    public static Cuboid6[][] occlusionBounds = new Cuboid6[3][6];
    
    static {
        for(int t = 0; t < 3; t++) {
            Cuboid6 selection = new Cuboid6(0, 0, 0, 1, (t+1)/8D, 1)
                .expand(-0.005);//subtract the box a little because we'd like things like posts to get first hit
            Cuboid6 occlusion = new Cuboid6(2/8D, 0, 2/8D, 6/8D, (t+1)/8D, 6/8D);
            for(int s = 0; s < 6; s++) {
                selectionBounds[t][s] = selection.copy().apply(Rotation.sideRotations[s].at(Vector3.center));
                occlusionBounds[t][s] = occlusion.copy().apply(Rotation.sideRotations[s].at(Vector3.center));
            }
        }
    }
    
    public byte side;
    /**
     * Currently split into 4 nybbles (from lowest)
     * 0 = Corner connections (this wire should connect around a corner to something external)
     * 1 = Straight connections (this wire should connect to something external)
     * 2 = Internal connections (this wire should connect to something internal)
     * 3 = Internal open connections (this wire is not blocked by a cover/edge part and *could* connect through side)
     * bit 16 = connection to the centerpart
     * 5 = Render corner connections. Like corner connections but set to low if the other wire part is smaller than this (they render to us not us to them)
     */
    public int connMap;
    
    public WirePart(int side) {
        this.side = (byte) side;
    }

    @Override
    public void save(NBTTagCompound tag) {
        super.save(tag);
        tag.setByte("side", side);
        tag.setInteger("connMap", connMap);
    }

    @Override
    public void load(NBTTagCompound tag) {
        super.load(tag);
        side = tag.getByte("side");
        connMap = tag.getInteger("connMap");
    }

    @Override
    public void writeDesc(MCDataOutput packet) {
        packet.writeByte(side);
        packet.writeInt(connMap);
    }

    @Override
    public void readDesc(MCDataInput packet) {
        side = packet.readByte();
        connMap = packet.readInt();
    }

    public void updateChange() {
        tile().markRender();
        tile().markDirty();
    }

    @Override
    public void onPartChanged() {
        if(!world().isRemote) {
            boolean changed = updateInternalConnections();
            if(updateOpenConnections())
                changed|=updateExternalConnections();
            if(changed) {
                sendConnUpdate();
                updatePowerState(true);
            }
        }
    }

    @Override
    public void onNeighborChanged() {
        if (!world().isRemote) {
            if(dropIfCantStay())
                return;
            
            if(updateExternalConnections()) {
                sendConnUpdate();
                updatePowerState(true);
            }
        }
    }
    
    public void sendConnUpdate() {
        tile().getWriteStream(this).writeByte(0).writeInt(connMap);
    }
    
    public void read(MCDataInput packet) {
        read(packet, packet.readUByte());
    }
    
    public void read(MCDataInput packet, int switch_key) {
        if(switch_key == 0) {
            connMap = packet.readInt();
            tile().markRender();
        }
    }
    
    @Override
    public void onChunkLoad()//do nothing on chunk load, we shouldn't have changed between saves
    {
    }
    
    @Override
    public void onWorldJoin()//when we're moved by a frame or something
    {
        onNeighborChanged();
    }
    
    public boolean canStay()
    {
        BlockCoord pos = new BlockCoord(getTile()).offset(side);
        return BasicWireUtils.canPlaceWireOnSide(world(), pos.x, pos.y, pos.z, ForgeDirection.getOrientation(side ^ 1), false);
    }
    
    public boolean dropIfCantStay()
    {
        if(!canStay())
        {
            drop();
            return true;
        }
        return false;
    }

    public void drop()
    {
        TileMultipart.dropItem(getItem(), world(), Vector3.fromTileEntityCenter(getTile()));
        tile().remPart(this);
    }
    
    /**
     * Recalculates connections to blocks outside this sapce
     * @return true if a new connection was added or one was removed
     */
    protected boolean updateExternalConnections() {
        int newConn = 0;
        for(int r = 0; r < 4; r++)
        {
            if(!maskOpen(r))
                continue;
            
            if(connectStraight(r))
                newConn|=0x10<<r;
            else {
                int cnrMode = connectCorner(r);
                if(cnrMode != 0) {
                    newConn|=1<<r;
                    if(cnrMode == 2)
                        newConn|=0x100000<<r;//render flag
                }
            }
        }
        
        if(newConn != (connMap & 0xF000FF))
        {
            int diff = connMap^newConn;
            connMap = (connMap&~0xF000FF)|newConn;
            
            //notify corner disconnections
            for(int r = 0; r < 4; r++)
                if((diff & 1<<r) != 0)
                    notifyCornerChange(r);
            
            return true;
        }
        return false;
    }
    
    /**
     * Recalculates connections to other parts within this space
     * @return true if a new connection was added or one was removed
     */
    protected boolean updateInternalConnections() {
        int newConn = 0;
        for(int r = 0; r < 4; r++)
            if(connectInternal(r))
                newConn|=0x100<<r;
        
        if(connectCenter())
            newConn|=0x10000;
        
        if(newConn != (connMap & 0x10F00))
        {
            connMap = (connMap&~0x10F00)|newConn;
            return true;
        }
        return false;
    }
    
    /**
     * Recalculates connections that can be made to other parts outside of this space
     * @return true if external connections should be recalculated
     */
    protected boolean updateOpenConnections() {
        int newConn = 0;
        for(int r = 0; r < 4; r++)
            if(connectionOpen(r))
                newConn|=0x1000<<r;
        
        if(newConn != (connMap & 0xF000))
        {
            connMap = (connMap&~0xF000)|newConn;
            return true;
        }
        return false;
    }
    
    /**
     * Tells this wire to recalculate it's power state from it's adjoining connections and propogate it's changes to other connected wires
     * @param force If set to true, this wire should tell others to re-evaluate themselves regardless of whether it's own power state changed.
     */
    protected abstract void updatePowerState(boolean force);

    public boolean connectionOpen(int r) {
        int absDir = Rotation.rotateSide(side, r);
        return (((TRedstoneTile)tile()).openConnections(absDir) & 1<<Rotation.rotationTo(absDir&6, side)) != 0;
    }

    public boolean connectStraight(int r) {
        int absDir = Rotation.rotateSide(side, r);
        
        BlockCoord pos = new BlockCoord(getTile());
        pos.offset(absDir);
        TileMultipart t = BasicUtils.getTileEntity(world(), pos, TileMultipart.class);
        if (t != null) {
            TMultiPart tp = t.partMap(side);
            if (tp instanceof IConnectable)
                return ((IConnectable) tp).connect(this, (r+2)%4);
        }
        
        return getExternalConnectionOveride(absDir);
    }

    public abstract boolean getExternalConnectionOveride(int absDir);

    /**
     * Return a corner connection state.
     * 0 = No connection
     * 1 = Physical connection
     * 2 = Render connection
     */
    public int connectCorner(int r) {
        int absDir = Rotation.rotateSide(side, r);
        
        BlockCoord pos = new BlockCoord(getTile());
        pos.offset(absDir);
        
        if(!canConnectThroughCorner(pos, absDir^1, side))
            return 0;
        
        pos.offset(side);
        TileMultipart t = BasicUtils.getTileEntity(world(), pos, TileMultipart.class);
        if (t != null) {
            TMultiPart tp = t.partMap(absDir^1);
            if (tp instanceof IConnectable) {
                boolean b = ((IConnectable) tp).connectCorner(this, Rotation.rotationTo(absDir^1, side^1));
                if(b) {
                    if(tp instanceof WirePart && ((WirePart)tp).getThickness() < getThickness())//let them connect to us
                        return 1;
                    
                    return 2;
                }
            }
        }
        return 0;
    }
    
    public boolean canConnectThroughCorner(BlockCoord pos, int side1, int side2) {
        if(world().isAirBlock(pos.x, pos.y, pos.z))
            return true;
        
        TileMultipart t = BasicUtils.getTileEntity(world(), pos, TileMultipart.class);
        if(t != null)
            return t.partMap(side1) == null && t.partMap(side2) == null && t.partMap(PartMap.edgeBetween(side1, side2)) == null;
        
        return false;
    }
    
    public boolean connectInternal(int r) {
        int absDir = Rotation.rotateSide(side, r);
        
        if(tile().partMap(PartMap.edgeBetween(absDir, side)) != null)
            return false;
        
        TMultiPart tp = tile().partMap(absDir);
        if (tp instanceof IConnectable)
            return ((IConnectable) tp).connectInternal(this, Rotation.rotationTo(absDir, side));
        if (tp instanceof IRedstonePart)
            return ((IRedstonePart) tp).canConnectRedstone(side);
        return false;
    }

    public boolean connectCenter() {
        TMultiPart t = tile().partMap(6);
        if (t instanceof IConnectable)
            return ((IConnectable) t).connect(this, -1);
        
        return false;
    }
    
    @Override
    public boolean connect(WirePart wire, int r)
    {
        if(canConnectToType(wire, r) && maskOpen(r))
        {
            int oldConn = connMap;
            connMap|=0x10<<r;
            if(oldConn != connMap)
                sendConnUpdate();
            return true;
        }
        return false;
    }
    
    @Override
    public boolean connectCorner(WirePart wire, int r)
    {
        if(canConnectToType(wire, r) && maskOpen(r))
        {
            int oldConn = connMap;
            connMap|=0x1<<r;
            if(wire.getThickness() >= getThickness())//render connection
                connMap|=0x100000<<r;
                
            if(oldConn != connMap)
                sendConnUpdate();
            return true;
        }
        return false;
    }
    
    @Override
    public boolean connectInternal(WirePart wire, int r)
    {
        if(canConnectToType(wire, r))
        {
            int oldConn = connMap;
            connMap|=0x100<<r;
            if(oldConn != connMap)
                sendConnUpdate();
            return true;
        }
        return false;
    }
    
    public void notifyCornerChange(int r) {
        int absDir = Rotation.rotateSide(side, r);
        
        BlockCoord pos = new BlockCoord(getTile()).offset(absDir).offset(side);
        world().notifyBlockOfNeighborChange(pos.x, pos.y, pos.z, getTile().getBlockType().blockID);
    }

    public void notifyStraightChange(int r) {
        int absDir = Rotation.rotateSide(side, r);
        
        BlockCoord pos = new BlockCoord(getTile()).offset(absDir);
        world().notifyBlockOfNeighborChange(pos.x, pos.y, pos.z, getTile().getBlockType().blockID);
    }

    public abstract boolean canConnectToType(WirePart wire, int r);
    
    public boolean maskConnects(int r) {
        return (connMap & 0x111 << r) != 0;
    }
    
    public boolean maskOpen(int r) {
        return (connMap & 0x1000 << r) != 0;
    }
    
    protected abstract boolean debug(EntityPlayer ply);

    protected void debugEffect_bonemeal() {
        world().playAuxSFX(2005, x(), y(), z(), 0);
    }

    protected void debugEffect_fireburst() {
        world().playAuxSFX(2004, x(), y(), z(), 0);
    }

    protected void debugEffect_potion() {
        world().playAuxSFX(2002, x(), y(), z(), 0);
    }

    protected void debugEffect_smoke() {
        world().playAuxSFX(2000, x(), y(), z(), 0);
    }

    /** START TILEMULTIPART INTERACTIONS **/
    @Override
    public float getStrength(MovingObjectPosition hit, EntityPlayer player) {
        return 4;
    }

    public ItemStack getItem() {
        return EnumWire.RED_ALLOY.getItemStack();
    }

    @Override
    public Iterable<ItemStack> getDrops() {
        return Arrays.asList(getItem());
    }

    @Override
    public ItemStack pickItem(MovingObjectPosition hit) {
        return getItem();
    }

    @Override
    public int getSlotMask() {
        return 1<<side;
    }

    @Override
    public Iterable<IndexedCuboid6> getSubParts() {
        return Arrays.asList(new IndexedCuboid6(0, selectionBounds[getThickness()][side]));
    }

    @Override
    public boolean occlusionTest(TMultiPart npart) {
        return NormalOcclusionTest.apply(this, npart);
    }

    @Override
    public Iterable<Cuboid6> getOcclusionBoxes() {
        return Arrays.asList(occlusionBounds[getThickness()][side]);
    }

    @Override
    public int redstoneConductionMap() {
        return 0;
    }

    @Override
    public boolean solid(int arg0) {
        return false;
    }

    @Override
    public boolean activate(EntityPlayer player, MovingObjectPosition hit, ItemStack held) {
        if (CommandDebug.WIRE_READING) {
            return debug(player);
        }
        return false;
    }

    public void renderStatic(RenderBlocks r) {
        CCRenderState.reset();
        CCRenderState.setBrightness(world(), x(), y(), z());
        CCRenderState.setColour(getColour());
        RenderWire.render(this);
        CCRenderState.setColour(-1);
    }
    
    public int getThickness() {
        return 0;
    }
    
    @SideOnly(Side.CLIENT)
    public Icon getIcon() {
        return EnumWire.RED_ALLOY.wireSprites[0];
    }
    
    public int getColour() {
        return -1;
    }
    
    @Override
    @SideOnly(Side.CLIENT)
    public void renderStatic(Vector3 pos, LazyLightMatrix olm, int pass) {
        if (pass == 0)
            renderStatic(null);
    }

    @Override
    public void drawBreaking(RenderBlocks r) {
        renderStatic(r);
    }
    
    public EnumWire getWireType() {
        return null;
    }
    
    @Override
    public void onRemoved() {
        super.onRemoved();
        
        if(!world().isRemote) {
            for(int r = 0; r < 4; r++)
                if(maskConnects(r)) {
                    if((connMap & 1<<r) != 0)
                        notifyCornerChange(r);
                    else if((connMap & 0x10<<r) != 0)
                        notifyStraightChange(r);
                }
        }
    }
    
    
}
