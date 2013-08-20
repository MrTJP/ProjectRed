package mrtjp.projectred.integration2;

import java.util.Arrays;

import mrtjp.projectred.ProjectRedIntegration;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.transmission.BasicWireUtils;
import mrtjp.projectred.transmission.IConnectable;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.MovingObjectPosition;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Vector3;
import codechicken.microblock.FaceMicroClass;
import codechicken.multipart.JCuboidPart;
import codechicken.multipart.JNormalOcclusion;
import codechicken.multipart.PartMap;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;

@SuppressWarnings({"rawtypes", "unchecked"})
public abstract class GatePart extends JCuboidPart implements JNormalOcclusion, IConnectable
{
    public byte orientation;
    public byte subID;
    public byte shape;
    
    public int connMap;
    
    public abstract GateLogic getLogic();
    
    public int side() {
        return orientation>>4;
    }
    
    public void setSide(int s) {
        orientation = (byte) (orientation&0xF | s<<4);
    }
    
    public int rotation() {
        return orientation&0xF;
    }
    
    public void setRotation(int r) {
        orientation = (byte) (orientation&0xF0 | r);
    }
    
    public int shape() {
        return shape&0xFF;
    }
    
    public void setShape(int s) {
        shape = (byte) s;
    }
    
    @Override
    public void save(NBTTagCompound tag) {
        tag.setByte("orient", orientation);
        tag.setByte("subID", subID);
        tag.setByte("shape", shape);
        tag.setShort("connMap", (short) connMap);
    }
    
    @Override
    public void load(NBTTagCompound tag) {
        orientation = tag.getByte("orient");
        subID = tag.getByte("subID");
        shape = tag.getByte("shape");
        connMap = tag.getShort("connMap")&0xFFFF;
    }
    
    @Override
    public void readDesc(MCDataInput packet) {
        orientation = packet.readByte();
        subID = packet.readByte();
        shape = packet.readByte();
    }
    
    @Override
    public void writeDesc(MCDataOutput packet) {
        packet.writeByte(orientation);
        packet.writeByte(subID);
        packet.writeByte(shape);
    }
    
    @Override
    public void onPartChanged(TMultiPart part) {
        if(!world().isRemote) {
            updateInternalConnections();
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
            boolean changed = updateInternalConnections();
            changed|=updateExternalConnections();//don't use || because it's fail fast
            if(changed)
                onChange();
        }
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

    @Override
    public void onMoved()
    {
        super.onMoved();
        onNeighborChanged();
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
    
    public ItemStack getItem() {
        return getGateType().getItem();
    }
    
    private EnumGate getGateType() {
        return EnumGate.VALID_GATES[subID&0xFF];
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
        
        if(!canConnectThroughCorner(pos, absDir^1, side()))
            return false;
        
        pos.offset(side());
        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if (t != null) {
            TMultiPart tp = t.partMap(absDir^1);
            if (tp instanceof IConnectable)
                return ((IConnectable) tp).connectCorner(this, Rotation.rotationTo(absDir^1, side()^1));
        }
        
        return false;
    }

    public boolean canConnectThroughCorner(BlockCoord pos, int side1, int side2) {
        if(world().isAirBlock(pos.x, pos.y, pos.z))
            return true;
        
        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if(t != null)
            return t.partMap(side1) == null && t.partMap(side2) == null && t.partMap(PartMap.edgeBetween(side1, side2)) == null;
        
        return false;
    }

    public boolean connectStraight(int r) {
        int absDir = Rotation.rotateSide(side(), r);
        
        BlockCoord pos = new BlockCoord(getTile()).offset(absDir);
        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if (t != null) {
            TMultiPart tp = t.partMap(side());
            if (tp instanceof IConnectable)
                return ((IConnectable) tp).connectStraight(this, (r+2)%4);
        }
        
        return connectStraightOverride(absDir);
    }

    public boolean connectStraightOverride(int absDir) {
        return false;
    }

    public boolean connectInternal(int r) {
        int absDir = Rotation.rotateSide(side(), r);
        
        if(tile().partMap(PartMap.edgeBetween(absDir, side())) != null)
            return false;
        
        TMultiPart tp = tile().partMap(absDir);
        if (tp instanceof IConnectable)
            return ((IConnectable) tp).connectInternal(this, Rotation.rotationTo(absDir, side()));
        
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
        return Arrays.asList(getBounds());
    }
    
    @Override
    public boolean activate(EntityPlayer player, MovingObjectPosition hit, ItemStack held) {
        if (held != null && held.getItem() == ProjectRedIntegration.itemScrewdriver) {
            if (player.isSneaking())
                configure();
            else
                rotate();
            
            return true;
        }
        
        return false;
    }

    public void configure() {
        int newShape = getLogic().cycleShape(shape());
        if(newShape != shape()) {
            setShape(newShape);
            notifyChange();
            onChange();
        }
    }

    public void rotate() {
        setRotation((rotation()+1)%4);
        notifyChange();
        onChange();
    }
    
    public void notifyChange() {
        tile().markDirty();
        tile().notifyPartChange(this);
        sendDescUpdate();
    }

    public void notifyInternalChange() {
        tile().markDirty();
        sendDescUpdate();
    }
    
    public void onChange() {
        getLogic().onChange(this);
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
        if(canConnectTo(part, r))
        {
            connMap|=0x100<<r;
            return true;
        }
        return false;
    }
    
    public boolean canConnectTo(IConnectable part, int r) {
        return getLogic().canConnectTo(this, part, r-rotation());
    }
    
    public int relRot(int side) {
        return Rotation.rotationTo(side(), side)-rotation();
    }
}
