package mrtjp.projectred.transmission;

import java.util.Arrays;

import mrtjp.projectred.core.BasicUtils;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.Icon;
import net.minecraft.util.MovingObjectPosition;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.lighting.LazyLightMatrix;
import codechicken.lib.raytracer.IndexedCuboid6;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Vector3;
import codechicken.microblock.IHollowConnect;
import codechicken.multipart.JNormalOcclusion;
import codechicken.multipart.NormalOcclusionTest;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TSlottedPart;
import codechicken.multipart.TileMultipart;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public abstract class ScaffoldWirePart extends TMultiPart implements IConnectable, TSlottedPart, JNormalOcclusion, IWirePart, IHollowConnect {
    public static Cuboid6[] boundingBoxes = new Cuboid6[7];
    
    static {
        double w = 2/8D;
        boundingBoxes[6] = new Cuboid6(0.5-w, 0.5-w, 0.5-w, 0.5+w, 0.5+w, 0.5+w);
        for(int s = 0; s < 6; s++)
            boundingBoxes[s] = new Cuboid6(0.5-w, 0, 0.5-w, 0.5+w, 0.5-w, 0.5+w)
                .apply(Rotation.sideRotations[s]);
    }
    
    /**
     * lowest 6 bits, flagged for an external connection to that side
     * next 6 bits, flagged for an internal connection to that side
     * next 6 bits, flagged for an open connection to that side
     * 
     * On the client, only the lowest 6 bits contain actual connections.
     */
    public int connMap;
    


    public void onPlaced(int meta) {
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

    public void read(MCDataInput packet, int switch_key) {
        if(switch_key == 0) {
            connMap = packet.readUByte();
            tile().markRender();
        }
    }
    
    @Override
    public void onPartChanged(TMultiPart part) {
        if(!world().isRemote) {
            WirePropogator.logCalculation();
            
            boolean changed = updateInternalConnections();
            if(updateOpenConnections())
                changed|=updateExternalConnections();
            if(changed) {
                sendConnUpdate();
                WirePropogator.propogateTo(this, FORCE);
            }
            else {
                WirePropogator.propogateTo(this, RISING);
            }
        }
    }
    
    @Override
    public void onNeighborChanged() {
        if (!world().isRemote) {
            WirePropogator.logCalculation();
            
            if(updateExternalConnections()) {
                sendConnUpdate();
                WirePropogator.propogateTo(this, FORCE);
            }
            else {
                WirePropogator.propogateTo(this, RISING);
            }
        }
    }
    
    @Override
    public void onAdded() {
        if(!world().isRemote) {
            updateOpenConnections();
            boolean changed = updateInternalConnections();
            changed|=updateExternalConnections();//don't use || because it's fail fast
            if(changed) {
                sendConnUpdate();
                WirePropogator.propogateTo(this, RISING);
            }
        }
    }

    @Override
    public void onRemoved() {
        super.onRemoved();
        
        if(!world().isRemote) {
            for(int s = 0; s < 6; s++)
                if((connMap & 1<<s) != 0)
                    notifyStraightChange(s);
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
    
    public int clientConnMap() {
        return connMap&0x3F | connMap>>6&0x3F;
    }

    public void sendConnUpdate() {
        tile().getWriteStream(this).writeByte(0).writeByte(clientConnMap());
    }
    

    /**
     * Recalculates connections to blocks outside this sapce
     * @return true if a new connection was added or one was removed
     */
    protected boolean updateExternalConnections() {
        int newConn = 0;
        for(int s = 0; s < 6; s++)
        {
            if(!maskOpen(s))
                continue;
            
            if(connectStraight(s))
                newConn|=1<<s;
        }
        
        if(newConn != (connMap & 0x3F))
        {
            connMap = (connMap&~0x3F)|newConn;
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
        for(int s = 0; s < 6; s++)
            if(connectInternal(s))
                newConn|=1<<(s+6);
        
        if(newConn != (connMap & 0xFC0)) {
            connMap = (connMap&~0xFC0)|newConn;
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
        for(int s = 0; s < 6; s++)
            if(connectionOpen(s))
                newConn|=1<<(s+12);
        
        if(newConn != (connMap & 0x3F000)) {
            connMap = (connMap&~0x3F000)|newConn;
            return true;
        }
        return false;
    }
    
    public boolean connectionOpen(int s) {
        TMultiPart facePart = tile().partMap(s);
        if(facePart != null && (!(facePart instanceof WirePart) || !canConnectToType((WirePart)facePart)))
            return false;
        
        return true;
    }

    public boolean connectStraight(int s) {
        BlockCoord pos = new BlockCoord(getTile()).offset(s);
        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if (t != null) {
            TMultiPart tp = t.partMap(6);
            if (tp instanceof IConnectable)
                return ((IConnectable) tp).connectStraight(this, s^1);
        }
        
        return connectStraightOverride(s);
    }

    public boolean connectStraightOverride(int s) {
        return false;
    }

    public boolean connectInternal(int s) {
        TMultiPart tp = tile().partMap(s);
        if (tp instanceof IConnectable)
            return ((IConnectable) tp).connectInternal(this, -1);
        
        return false;
    }
    
    @Override
    public boolean connectStraight(IWirePart wire, int s) {
        if(canConnectToType(wire) && maskOpen(s)) {
            int oldConn = connMap;
            connMap|=1<<s;
            if(oldConn != connMap)
                sendConnUpdate();
            return true;
        }
        return false;
    }
    
    @Override
    public boolean connectInternal(IWirePart wire, int s) {
        if(canConnectToType(wire)) {
            int oldConn = connMap;
            connMap|=1<<(s+6);
            if(oldConn != connMap)
                sendConnUpdate();
            return true;
        }
        return false;
    }
    
    @Override
    public boolean connectCorner(WirePart wire, int r) {
        return false;
    }
    
    public abstract boolean canConnectToType(IWirePart wire);

    public void notifyStraightChange(int s) {
        BlockCoord pos = new BlockCoord(getTile()).offset(s);
        world().notifyBlockOfNeighborChange(pos.x, pos.y, pos.z, getTile().getBlockType().blockID);
    }

    public boolean maskConnects(int s) {
        return (connMap & 0x41 << s) != 0;
    }
    
    public boolean maskOpen(int s) {
        return (connMap & 0x1000 << s) != 0;
    }
    
    public void propogate(TMultiPart prev, int mode) {
        if(mode != FORCED)
            WirePropogator.addPartChange(this);

        for(int s = 0; s < 6; s++)
            if((connMap & 1<<s) != 0)
                propogateStraight(s, prev, mode);
            else if((connMap & 1<<(s+6)) != 0)
                propogateInternal(s, prev, mode);
    }

    public void propogateStraight(int s, TMultiPart prev, int mode) {
        BlockCoord pos = new BlockCoord(getTile()).offset(s);

        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if (t != null) {
            TMultiPart tp = t.partMap(6);
            if(tp == prev)
                return;
            if(propogateTo(tp, mode))
                return;
        }

        WirePropogator.addNeighborChange(pos);
    }
    
    public void propogateInternal(int s, TMultiPart prev, int mode) {
        TMultiPart tp = tile().partMap(s);
        if(tp == prev)
            return;
        propogateTo(tp, mode);
    }
    
    public boolean propogateTo(TMultiPart part, int mode) {
        if(part instanceof IWirePart) {
            WirePropogator.propogateTo((IWirePart) part, this, mode);
            return true;
        }
        
        return false;
    }
    
    @Override
    public void onSignalUpdate() {
        tile().markDirty();
    }

    public EnumWire getWireType() {
        return null;
    }

    public int getThickness() {
        return getWireType().thickness;
    }
    
    /** START TILEMULTIPART INTERACTIONS **/
    @Override
    public float getStrength(MovingObjectPosition hit, EntityPlayer player) {
        return 4;
    }

    public ItemStack getItem() {
        return getWireType().getJacketedItemStack();
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
        return 6;
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

    @SideOnly(Side.CLIENT)
    public Icon getIcon() {
        return getWireType().wireSprites[0];
    }
    
    public int getColour() {
        return -1;
    }
    
    @Override
    @SideOnly(Side.CLIENT)
    public void renderStatic(Vector3 pos, LazyLightMatrix olm, int pass) {
        if (pass == 0) {
            CCRenderState.reset();
            CCRenderState.setBrightness(world(), x(), y(), z());
            CCRenderState.useModelColours(true);
            RenderScaffoldedWire.render(this);
            CCRenderState.setColour(-1);
        }
    }
    
    @Override
    @SideOnly(Side.CLIENT)
    public void drawBreaking(RenderBlocks renderBlocks) {
        CCRenderState.reset();
        RenderScaffoldedWire.renderBreakingOverlay(renderBlocks.overrideBlockTexture, this);
    }
    
    @Override
    public boolean doesTick() {
        return false;
    }
    
    @Override
    public int getHollowSize() {
        return 8;
    }
}
