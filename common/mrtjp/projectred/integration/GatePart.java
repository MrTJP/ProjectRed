package mrtjp.projectred.integration;

import java.util.Arrays;

import mrtjp.projectred.ProjectRedIntegration;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.BasicWireUtils;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.MovingObjectPosition;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.lighting.LazyLightMatrix;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.TextureUtils;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Vector3;
import codechicken.microblock.FaceMicroClass;
import codechicken.multipart.JCuboidPart;
import codechicken.multipart.JNormalOcclusion;
import codechicken.multipart.NormalOcclusionTest;
import codechicken.multipart.PartMap;
import codechicken.multipart.TFacePart;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TickScheduler;
import codechicken.multipart.TileMultipart;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SuppressWarnings({"rawtypes", "unchecked"})
public abstract class GatePart extends JCuboidPart implements JNormalOcclusion, IConnectable, TFacePart
{
    public static Cuboid6[][] oBoxes = new Cuboid6[6][2];
    
    static
    {
        oBoxes[0][0] = new Cuboid6(1/8D, 0, 0, 7/8D, 1/8D, 1);
        oBoxes[0][1] = new Cuboid6(0, 0, 1/8D, 1, 1/8D, 7/8D);
        for(int s = 1; s < 6; s++) {
            Transformation t = Rotation.sideRotations[s].at(Vector3.center);
            oBoxes[s][0] = oBoxes[0][0].copy().apply(t);
            oBoxes[s][1] = oBoxes[0][1].copy().apply(t);
        }
    }
    
    public byte orientation;
    public byte subID;
    public byte shape;
    
    public int connMap;
    public long schedTime;
    
    public abstract GateLogic getLogic();
    
    public int side() {
        return orientation>>2;
    }
    
    public void setSide(int s) {
        orientation = (byte) (orientation&0x3 | s<<2);
    }
    
    public int rotation() {
        return orientation&0x3;
    }
    
    public void setRotation(int r) {
        orientation = (byte) (orientation&0xFC | r);
    }
    
    public int shape() {
        return shape&0xFF;
    }
    
    public void setShape(int s) {
        shape = (byte) s;
    }
    
    public Transformation rotationT() {
        return Rotation.sideOrientation(side(), rotation()).at(Vector3.center);
    }

    public void onPlaced(EntityPlayer player, int side, int meta) {
        subID = (byte) meta;
        setSide(side^1);
        setRotation(Rotation.getSidedRotation(player, side));
    }
    
    @Override
    public void save(NBTTagCompound tag) {
        tag.setByte("orient", orientation);
        tag.setByte("subID", subID);
        tag.setByte("shape", shape);
        tag.setShort("connMap", (short) connMap);
        tag.setLong("schedTime", schedTime);
    }
    
    @Override
    public void load(NBTTagCompound tag) {
        orientation = tag.getByte("orient");
        subID = tag.getByte("subID");
        shape = tag.getByte("shape");
        connMap = tag.getShort("connMap")&0xFFFF;
        schedTime = tag.getLong("schedTime");
    }
    
    @Override
    public void readDesc(MCDataInput packet) {
        orientation = packet.readByte();
        subID = packet.readByte();
        shape = packet.readByte();
    }
    
    public void read(MCDataInput packet) {
        read(packet, packet.readUByte());
    }

    public void read(MCDataInput packet, int switch_key) {
        if(switch_key == 0) {
            orientation = packet.readByte();
            tile().markRender();
        }
        else if(switch_key == 1) {
            shape = packet.readByte();
            tile().markRender();
        }
    }
    
    @Override
    public void writeDesc(MCDataOutput packet) {
        packet.writeByte(orientation);
        packet.writeByte(subID);
        packet.writeByte(shape);
    }
    
    @Override
    public void scheduleTick(int ticks) {
        if(schedTime < 0) {
            schedTime = TickScheduler.getSchedulerTime(world())+ticks;
        }
    }
        
    private void processScheduled() {
        if(schedTime >= 0 && TickScheduler.getSchedulerTime(world()) >= schedTime) {
            schedTime = -1;
            scheduledTick();
        }
    }
    
    public void onChange() {
        processScheduled();
        getLogic().onChange(this);
    }
    
    @Override
    public void update() {
        if(!world().isRemote)
            processScheduled();
        
        getLogic().onTick(this);
    }
    
    @Override
    public void onPartChanged(TMultiPart part) {
        if(!world().isRemote) {
            if(updateInternalConnections())
                onChange();
        }
    }

    @Override
    public void onNeighborChanged() {
        if (!world().isRemote) {
            if(dropIfCantStay())
                return;
            
            updateExternalConnections();
            onChange();
        }
    }
    
    @Override
    public void onAdded() {
        super.onAdded();
        if(!world().isRemote) {
            getLogic().setup(this);
            updateConnections();
            onChange();
        }
    }

    @Override
    public void onRemoved() {
        super.onRemoved();
        if(!world().isRemote) {
            notifyNeighbors(0xF);
        }
    }

    public boolean canStay()
    {
        BlockCoord pos = new BlockCoord(getTile()).offset(side());
        return BasicWireUtils.canPlaceWireOnSide(world(), pos.x, pos.y, pos.z, ForgeDirection.getOrientation(side() ^ 1), false);
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
        
    public EnumGate getGateType() {
        return EnumGate.VALID_GATES[subID&0xFF];
    }
    
    protected void updateConnections() {
        updateInternalConnections();
        updateExternalConnections();
    }

    /**
     * Recalculates connections to blocks outside this sapce
     * @return true if a new connection was added or one was removed
     */
    protected boolean updateExternalConnections() {
        int newConn = 0;
        for(int r = 0; r < 4; r++)
        {
            if(connectStraight(r))
                newConn|=0x10<<r;
            else if(connectCorner(r))
                newConn|=1<<r;
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
        
        if(newConn != (connMap & 0x10F00))
        {
            connMap = (connMap&~0x10F00)|newConn;
            return true;
        }
        return false;
    }
    
    public boolean connectCorner(int r) {
        int absDir = Rotation.rotateSide(side(), r);
        
        BlockCoord pos = new BlockCoord(getTile());
        pos.offset(absDir);
        
        if(!BasicWireUtils.canConnectThroughCorner(world(), pos, absDir^1, side()))
            return false;
        
        pos.offset(side());
        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if (t != null) {
            TMultiPart tp = t.partMap(absDir^1);
            if (tp instanceof IConnectable) {
                IConnectable conn = (IConnectable) tp;
                int r2 = Rotation.rotationTo(absDir^1, side()^1);
                return canConnectTo(conn, r) && conn.canConnectCorner(r2) &&
                        conn.connectCorner(this, r2);
            }
        }
        
        return false;
    }

    public boolean connectStraight(int r) {
        int absDir = Rotation.rotateSide(side(), r);
        
        BlockCoord pos = new BlockCoord(getTile()).offset(absDir);
        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if (t != null) {
            TMultiPart tp = t.partMap(side());
            if (tp instanceof IConnectable) {
                IConnectable conn = (IConnectable) tp;
                return canConnectTo(conn, r) && conn.connectStraight(this, (r+2)%4);
            }
        }
        
        return false;
    }

    public boolean connectInternal(int r) {
        int absDir = Rotation.rotateSide(side(), r);
        
        if(tile().partMap(PartMap.edgeBetween(absDir, side())) != null)
            return false;
        
        TMultiPart tp = tile().partMap(absDir);
        if (tp instanceof IConnectable) {
            IConnectable conn = (IConnectable) tp;
            return canConnectTo(conn, r) && conn.connectInternal(this, Rotation.rotationTo(absDir, side()));
        }
        
        return false;
    }

    public void notifyCornerChange(int r) {
        int absDir = Rotation.rotateSide(side(), r);
        
        BlockCoord pos = new BlockCoord(getTile()).offset(absDir).offset(side());
        world().notifyBlockOfNeighborChange(pos.x, pos.y, pos.z, getTile().getBlockType().blockID);
    }

    public void notifyStraightChange(int r) {
        int absDir = Rotation.rotateSide(side(), r);
        
        BlockCoord pos = new BlockCoord(getTile()).offset(absDir);
        world().notifyBlockOfNeighborChange(pos.x, pos.y, pos.z, getTile().getBlockType().blockID);
    }

    public boolean maskConnects(int r) {
        return (connMap & 0x111 << r) != 0;
    }
    
    @Override
    public Cuboid6 getBounds() {
        return FaceMicroClass.aBounds()[0x10|side()];
    }
    
    @Override
    public Iterable<Cuboid6> getOcclusionBoxes() {
        return Arrays.asList(oBoxes[side()]);
    }
    
    @Override
    public boolean occlusionTest(TMultiPart npart) {
        return NormalOcclusionTest.apply(this, npart);
    }
    
    @Override
    public float getStrength(MovingObjectPosition hit, EntityPlayer player) {
        return hit.sideHit == 1 ? 1.75f : 1.5f;
    }

    public ItemStack getItem() {
        return getGateType().getItemStack();
    }
    
    @Override
    public ItemStack pickItem(MovingObjectPosition hit) {
        return getItem();
    }

    @Override
    public Iterable<ItemStack> getDrops() {
        return Arrays.asList(getItem());
    }

    @Override
    public boolean activate(EntityPlayer player, MovingObjectPosition hit, ItemStack held) {
        if(getLogic().activate(this, player, held))
            return true;
        
        if (held != null && held.getItem() == ProjectRedIntegration.itemScrewdriver) {
            if(!world().isRemote) {
                if (player.isSneaking())
                    configure();
                else
                    rotate();
                held.damageItem(1, player);
            }
            
            return true;
        }
        
        return false;
    }

    public void configure() {
        boolean changed = getLogic().cycleShape(this);
        if(changed) {
            updateConnections();
            tile().markDirty();
            sendShapeUpdate();
            notifyNeighbors(0xF);
            onChange();
        }
    }
    
    public void rotate() {
        setRotation((rotation()+1)%4);
        
        updateConnections();
        tile().markDirty();
        sendOrientationUpdate();
        notifyNeighbors(0xF);
        onChange();
    }
    
    public MCDataOutput getWriteStream(int switch_key) {
        return tile().getWriteStream(this).writeByte(switch_key);
    }
    
    public void sendShapeUpdate() {
        getWriteStream(1).writeByte(shape);
    }
    
    public void sendOrientationUpdate() {
        getWriteStream(0).writeByte(orientation);
    }
    
    /**
     * Notify neighbor blocks
     * @param mask A bitmask of absolute rotation sides to notify
     */
    public void notifyNeighbors(int mask) {
        for(int r = 0; r < 4; r++)
            if((connMap & 1<<r) != 0)
                notifyCornerChange(r);
            else if((connMap & 0x10<<r) != 0)
                notifyStraightChange(r);
    }
    
    public void scheduledTick() {
        getLogic().scheduledTick(this);
    }
    
    @Override
    public boolean connectCorner(IConnectable part, int r)
    {
        if(canConnectTo(part, r))
        {
            connMap|=0x1<<r;
            return true;
        }
        return false;
    }

    @Override
    public boolean connectStraight(IConnectable part, int r)
    {
        if(canConnectTo(part, r))
        {
            connMap|=0x10<<r;
            return true;
        }
        return false;
    }
    
    @Override
    public boolean connectInternal(IConnectable part, int r)
    {
        if(r < 0)
            return false;
        
        if(canConnectTo(part, r))
        {
            connMap|=0x100<<r;
            return true;
        }
        return false;
    }
    
    @Override
    public boolean canConnectCorner(int r) {
        return false;
    }
    
    public int toInternal(int absRot) {
        return (absRot+6-rotation())%4;
    }
    
    public int toAbsolute(int r) {
        return (r+rotation()+2)%4;
    }
    
    public static int shiftMask(int mask, int r) {
        return (mask<<r | mask>>(4-r)) & 0xF;
    }
    
    public static int flipMaskZ(int mask) {
        return mask & 5 | mask<<2 & 8 | mask>>2 & 2;
    }
    
    public int toAbsoluteMask(int mask) {
        return shiftMask(mask, toAbsolute(0));
    }
    
    public int toInternalMask(int mask) {
        return shiftMask(mask, toInternal(0));
    }
    
    public int relRot(int side) {
        return toInternal(Rotation.rotationTo(side(), side));
    }
    
    public boolean canConnectTo(IConnectable part, int r) {
        return getLogic().canConnectTo(this, part, toInternal(r));
    }
    
    @Override
    @SideOnly(Side.CLIENT)
    public void renderStatic(Vector3 pos, LazyLightMatrix olm, int pass) {
        if(pass == 0) {
            TextureUtils.bindAtlas(0);
            CCRenderState.reset();
            CCRenderState.setBrightness(world(), x(), y(), z());
            CCRenderState.useModelColours(true);
            RenderGate.renderStatic(this);
            CCRenderState.setColour(-1);
        }
    }
    
    @Override
    @SideOnly(Side.CLIENT)
    public void renderDynamic(Vector3 pos, float frame, int pass) {
        if(pass == 0)
            TextureUtils.bindAtlas(0);
            RenderGate.renderDynamic(this, pos, frame);
    }
    
    public int getSlotMask() {
        return 1<<side();
    }
    
    @Override
    public int redstoneConductionMap() {
        return 0;
    }
    
    @Override
    public boolean solid(int side) {
        return false;
    }
}
