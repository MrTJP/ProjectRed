package mrtjp.projectred.integration;

import java.util.Arrays;

import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.integration.ArrayCommons.ITopArrayWire;
import mrtjp.projectred.transmission.IRedwireEmitter;
import mrtjp.projectred.transmission.IRedwirePart;
import mrtjp.projectred.transmission.IWirePart;
import mrtjp.projectred.transmission.WirePropogator;
import net.minecraft.block.Block;
import net.minecraft.nbt.NBTTagCompound;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.RedstoneInteractions;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;

public class RowGatePart extends SimpleGatePart implements IRedwirePart, ITopArrayWire
{
    public static Cuboid6[][] oBoxes = new Cuboid6[6][2];
    public static Cuboid6[] cBoxes = new Cuboid6[6];
    
    static
    {
        oBoxes[0][0] = new Cuboid6(1/8D, 0, 0, 7/8D, 6/8D, 1);
        oBoxes[0][1] = new Cuboid6(0, 0, 1/8D, 1, 6/8D, 7/8D);
        cBoxes[0] = new Cuboid6(0, 0, 0, 1, 6/8D, 1);
        for(int s = 1; s < 6; s++) {
            Transformation t = Rotation.sideRotations[s].at(Vector3.center);
            oBoxes[s][0] = oBoxes[0][0].copy().apply(t);
            oBoxes[s][1] = oBoxes[0][1].copy().apply(t);
            cBoxes[s] = cBoxes[0].copy().apply(t);
        }
    }
    
    public byte signal;
    
    @Override
    public String getType() {
        return "pr_rgate";
    }

    @Override
    public void save(NBTTagCompound tag) {
        super.save(tag);
        tag.setByte("signal", signal);
    }
    
    @Override
    public void load(NBTTagCompound tag) {
        super.load(tag);
        signal = tag.getByte("signal");
    }
    
    @Override
    public void writeDesc(MCDataOutput packet) {
        super.writeDesc(packet);
        packet.writeByte(signal);
    }
    
    @Override
    public void readDesc(MCDataInput packet) {
        super.readDesc(packet);
        signal = packet.readByte();
    }
    
    @Override
    public void read(MCDataInput packet, int switch_key) {
        if(switch_key == 11) {
            signal = packet.readByte();
            if(Configurator.staticGates)
                tile().markRender();
        }
        else
            super.read(packet, switch_key);
    }
    
    @Override
    public void updateAndPropogate(TMultiPart prev, int mode) {
        int wireMask = wireMask(prev);
        if((wireMask & 2) != 0)
            _updateAndPropogate(prev, mode);
        else
            WirePropogator.addNeighborChange(new BlockCoord(tile()));
    }

    private void _updateAndPropogate(TMultiPart prev, int mode) {
        int oldSignal = signal & 0xFF;
        if(mode == DROPPING && oldSignal == 0)
            return;

        int newSignal = calculateSignal();
        if(newSignal < oldSignal) {
            if(newSignal > 0)
                WirePropogator.propogateAnalogDrop(this);
            
            signal = 0;
            propogate(prev, DROPPING);
        }
        else if(newSignal > oldSignal) {
            signal = (byte)newSignal;
            if(mode == DROPPING)
                propogate(null, RISING);
            else
                propogate(prev, RISING);
        }
        else {
            if(mode == DROPPING)
                propogateTo(prev, RISING);
            else if(mode == FORCE)
                propogate(prev, FORCED);
        }
    }

    public int calculateSignal() {
        WirePropogator.setWiresProvidePower(false);
        WirePropogator.redwiresProvidePower = false;
        
        int s = 0;
        int i = 0;
        for(int r = 0; r < 4; r++)
            if(toInternal(r)%2 != 0) {
                if((connMap & 1<<r) != 0)
                    i = calculateCornerSignal(r);
                else
                    i = calculateStraightSignal(r);
                
                if(i > s)
                    s = i;
            }
        
        WirePropogator.setWiresProvidePower(true);
        WirePropogator.redwiresProvidePower = true;
        
        return s;
    }

    public int calculateCornerSignal(int r) {
        int absDir = Rotation.rotateSide(side(), r);
        
        BlockCoord cnrPos = new BlockCoord(tile()).offset(absDir);
        BlockCoord pos = cnrPos.copy().offset(side());
        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if (t != null)
            return getPartSignal(t.partMap(absDir^1), Rotation.rotationTo(absDir^1, side()^1));
        
        return 0;
    }

    public int calculateStraightSignal(int r) {
        int absDir = Rotation.rotateSide(side(), r);
        int s = 0;
        
        BlockCoord pos = new BlockCoord(tile()).offset(absDir);
        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if (t != null && (connMap & 0x10<<r) != 0) {
            TMultiPart tp = t.partMap(side());
            if(tp != null)
                s = getPartSignal(tp, (r+2)%4);
        }
        else {
            int blockID = world().getBlockId(pos.x, pos.y, pos.z);
            if(blockID == Block.redstoneWire.blockID)
                return world().getBlockMetadata(pos.x, pos.y, pos.z)-1;
        }
        
        int i = calculateRedstoneSignal(r);
        if(i > s)
            s = i;
        
        return s;
    }
    
    public int calculateRedstoneSignal(int r) {
        int absDir = Rotation.rotateSide(side(), r);
        
        int i = RedstoneInteractions.getPowerTo(this, absDir)*17;
        if(i > 0)
            return i;
        
        BlockCoord pos = new BlockCoord(tile()).offset(absDir);
        return world().getIndirectPowerLevelTo(pos.x, pos.y, pos.z, absDir)*17;
    }

    public int getPartSignal(TMultiPart part, int r) {
        if(part instanceof IRedwirePart && ((IRedwirePart) part).isWireSide(r))
            return ((IRedwirePart) part).getRedwireSignal(r) - 1;
        else if(part instanceof IRedwireEmitter)
            return ((IRedwireEmitter) part).getRedwireSignal(r);
        
        return 0;
    }

    @Override
    public void onSignalUpdate() {
        tile().markDirty();
        super.onChange();
        getWriteStream(11).writeByte(signal);
    }
    
    public void propogate(TMultiPart prev, int mode) {
        if(mode != FORCED)
            WirePropogator.addPartChange(this);
        
        for(int r = 0; r < 4; r++)
            if(toInternal(r)%2 != 0) {
                if((connMap & 1<<r) != 0)
                    propogateCorner(r, prev, mode);
                else
                    propogateStraight(r, prev, mode);
            }
    }

    public void propogateCorner(int r, TMultiPart prev, int mode) {
        int absDir = Rotation.rotateSide(side(), r);
        BlockCoord pos = new BlockCoord(tile()).offset(absDir).offset(side());

        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if (t != null) {
            TMultiPart tp = t.partMap(absDir^1);
            if(tp == prev)
                return;
            if(propogateTo(tp, mode))
                return;
        }
        
        WirePropogator.addNeighborChange(pos);
    }
    
    public void propogateStraight(int r, TMultiPart prev, int mode) {
        int absDir = Rotation.rotateSide(side(), r);
        BlockCoord pos = new BlockCoord(tile()).offset(absDir);

        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if (t != null) {
            TMultiPart tp = t.partMap(side());
            if(tp == prev)
                return;
            if(propogateTo(tp, mode))
                return;
        }

        WirePropogator.addNeighborChange(pos);
    }

    public int wireMask(TMultiPart propogator) {
        if(propogator.tile() == null)
            return 3;
        
        if(sideDiff(propogator) == Rotation.rotateSide(side(), toAbsolute(0))>>1)
            return 1;
        
        return 2;
    }
    
    public int sideDiff(TMultiPart part) {
        int a = side()>>1;
        if(a != 0 && y() != part.y())
            return 0;
        if(a != 1 && z() != part.z())
            return 1;
        
        return 2;
    }

    public boolean propogateTo(TMultiPart part, int mode) {
        if(part instanceof IWirePart) {
            WirePropogator.propogateTo((IWirePart) part, this, mode);
            return true;
        }
        
        return false;
    }
    
    @Override
    public int getRedwireSignal(int side) {
        int r = toInternal(side);
        return r%2 == 0 ? 
                getLogic().getOutput(this, r)*17 : 
                signal & 0xFF;
    }
    
    @Override
    public void onChange() {
        super.onChange();
        WirePropogator.propogateTo(this, RISING);
    }
    
    @Override
    public boolean canConnectRedstone(int side) {
        if((side&6) == (side()&6))
            return false;
        
        int r = relRot(side);
        if(r%2 != 0)
            return true;
        
        return getLogic().canConnect(this, r);
    }
    
    @Override
    public int weakPowerLevel(int side) {
        if((side&6) == (side()&6))
            return 0;
        
        int r = relRot(side);
        if(r%2 != 0)
            return rsLevel(signal & 0xFF);
        
        return super.weakPowerLevel(side);
    }
    
    public int rsLevel(int i) {
        if(WirePropogator.redwiresProvidePower)
            return (i+16)/17;
        
        return 0;
    }
    
    @Override
    public boolean isWireSide(int side) {
        if(side < 0)
            return false;
        
        return toInternal(side)%2 != 0;
    }
    
    @Override
    public Cuboid6 getBounds() {
        return cBoxes[side()];
    }
    
    @Override
    public Iterable<Cuboid6> getOcclusionBoxes() {
        return Arrays.asList(oBoxes[side()]);
    }
}
