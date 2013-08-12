package mrtjp.projectred.transmission;

import java.util.ArrayList;
import java.util.HashSet;

import mrtjp.projectred.core.BasicUtils;
import net.minecraft.block.Block;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.ChatMessageComponent;
import net.minecraft.world.World;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Rotation;
import codechicken.multipart.IFaceRedstonePart;
import codechicken.multipart.IRedstonePart;
import codechicken.multipart.RedstoneInteractions;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;
import codechicken.multipart.handler.MultipartProxy;

public abstract class RedwirePart extends WirePart implements IRedwirePart, IFaceRedstonePart {

    public static boolean redwiresProvidePower = true;
    
    public byte strength;
    
    public RedwirePart(int side) {
        super(side);
    }
    
    @Override
    public void save(NBTTagCompound tag) {
        super.save(tag);
        tag.setByte("strength", strength);
    }
    
    @Override
    public void load(NBTTagCompound tag) {
        super.load(tag);
        strength = tag.getByte("strength");
    }
    
    @Override
    public void writeDesc(MCDataOutput packet) {
        super.writeDesc(packet);
        packet.writeByte(strength);
    }
    
    @Override
    public void readDesc(MCDataInput packet) {
        super.readDesc(packet);
        strength = packet.readByte();
    }
    
    @Override
    public void read(MCDataInput packet, int switch_key) {
        if(switch_key == 10)
            strength = packet.readByte();
        else
            super.read(packet, switch_key);
    }
    
    @Override
    public int getFace() {
        return side;
    }
    
    @Override
    public int strongPowerLevel(int side) {
        if(side == this.side)
            return rsLevel();
        
        return 0;
    }
    
    @Override
    public int weakPowerLevel(int side) {
        return rsLevel();
    }
    
    @Override
    public boolean canConnectRedstone(int side) {
        return true;
    }
    
    public int rsLevel() {
        if(redwiresProvidePower)
            return (strength&0xFF)>>4;
        
        return 0;
    }
    
    @Override
    protected boolean debug(EntityPlayer ply) {
        ply.sendChatToPlayer(ChatMessageComponent.func_111077_e((world().isRemote ? "Client" : "Server") + " signal strength: " + strength));
        return true;
    }
    
    @Override
    public boolean canConnectToType(WirePart wire, int r) {
        return wire instanceof RedwirePart;
    }
    
    @Override
    public boolean getExternalConnectionOveride(int absDir) {
        return (RedstoneInteractions.otherConnectionMask(world(), x(), y(), z(), absDir, false) & 
                RedstoneInteractions.connectionMask(this, absDir)) != 0;
    }
    
    public void sendStrengthUpdate() {
        tile().getWriteStream(this).writeByte(10).writeByte(strength);
    }
    
    public void updateAndPropogate(TMultiPart prev, boolean force) {

        int newStrength = calculateStrength();
        boolean changed = newStrength != (strength & 0xFF);
        strength = (byte)newStrength;
        if(changed)
            sendStrengthUpdate();
        else if(!force)
            return;
        
        WirePropogator.beginPropogating();
        
        WirePropogator.partChanges.add(this);
        
        for(int r = 0; r < 4; r++)
            if((connMap & 1<<r) != 0)
                propogateCorner(r, prev);
            else if((connMap & 0x10<<r) != 0)
                propogateStraight(r, prev);
            else if((connMap & 0x100<<r) != 0)
                propogateInternal(r, prev);
        
        if((connMap & 0x10000) != 0)
            propogateCenter(prev);
        
        propogateOther();
        
        WirePropogator.endPropogating(world());
    }

    public void propogateOther() {
    }

    public void propogateCorner(int r, TMultiPart prev) {
        int absDir = Rotation.rotateSide(side, r);
        BlockCoord pos = new BlockCoord(getTile()).offset(absDir).offset(side);

        TileMultipart t = BasicUtils.getTileEntity(world(), pos, TileMultipart.class);
        if (t != null) {
            TMultiPart tp = t.partMap(absDir^1);
            if(tp == prev)
                return;
            if(tp instanceof IWirePart) {
                ((IWirePart) tp).updateAndPropogate(this, false);
                return;
            }
        }
        
        WirePropogator.neighborChanges.add(pos);
    }
    
    public void propogateStraight(int r, TMultiPart prev) {
        int absDir = Rotation.rotateSide(side, r);
        BlockCoord pos = new BlockCoord(getTile()).offset(absDir);

        TileMultipart t = BasicUtils.getTileEntity(world(), pos, TileMultipart.class);
        if (t != null) {
            TMultiPart tp = t.partMap(side);
            if(tp == prev)
                return;
            if(tp instanceof IWirePart) {
                ((IWirePart) tp).updateAndPropogate(this, false);
                return;
            }
        }
        
        //no need for neighbour change, onPartChanged will do it for us
    }
    
    public void propogateInternal(int r, TMultiPart prev) {
        int absDir = Rotation.rotateSide(side, r);
        TMultiPart tp = tile().partMap(absDir);
        if(tp == prev)
            return;
        if(tp instanceof IWirePart)
            ((IWirePart) tp).updateAndPropogate(this, false);
    }
    
    public void propogateCenter(TMultiPart prev) {
        TMultiPart tp = tile().partMap(6);
        if(tp == prev)
            return;
        if(tp instanceof IWirePart)
            ((IWirePart) tp).updateAndPropogate(this, false);
    }

    public int calculateStrength() {
        WirePropogator.setWiresProvidePower(false);
        redwiresProvidePower = false;
        
        int s = 0;
        for(int r = 0; r < 4; r++)
            if(maskConnects(r)) {
                int i;
                if((connMap & 1<<r) != 0)
                    i = calculateCornerStrength(r);
                else if((connMap & 0x10<<r) != 0)
                    i = calculateStraightStrength(r);
                else
                    i = calculateInternalStrength(r);
                
                if(i > s)
                    s = i;
            }
        
        int i = calculateUndersideStrength();
        if(i > s)
            s = i;
        
        WirePropogator.setWiresProvidePower(true);
        redwiresProvidePower = true;
        
        return s;
    }

    public int calculateUndersideStrength() {
        return 0;
    }

    public int calculateInternalStrength(int r) {
        int absDir = Rotation.rotateSide(side, r);
        
        TMultiPart tp = tile().partMap(absDir);
        int i = getPartStrength(tp, Rotation.rotationTo(absDir, side));
        if(i > 0)
            return i;
        
        if(tp instanceof IRedstonePart) {
            IRedstonePart rp = (IRedstonePart) tp;
            return Math.max(rp.strongPowerLevel(side), rp.weakPowerLevel(side)) << 4;
        }
        
        return 0;
    }

    public int calculateStraightStrength(int r) {
        int absDir = Rotation.rotateSide(side, r);
        
        BlockCoord pos = new BlockCoord(getTile());
        pos.offset(absDir);
        TileMultipart t = BasicUtils.getTileEntity(world(), pos, TileMultipart.class);
        if (t != null) {
            int i = getPartStrength(t.partMap(side), (r+2)%4);
            if(i > 0)
                return i;
        }

        int blockID = world().getBlockId(pos.x, pos.y, pos.z);
        if(blockID == Block.redstoneWire.blockID)
            return world().getBlockMetadata(pos.x, pos.y, pos.z);
        
        return RedstoneInteractions.getPowerTo(this, absDir)*17;
    }
    
    public int getPartStrength(TMultiPart part, int r) {
        if(part instanceof RedwirePart)
            return (((RedwirePart) part).strength & 0xFF) - 1;
        else if(part instanceof IRedstoneEmitter)
            return ((IRedstoneEmitter) part).getEmittedSignalStrength(r);
        
        return 0;
    }

    public int calculateCornerStrength(int r) {
        int absDir = Rotation.rotateSide(side, r);
        
        BlockCoord pos = new BlockCoord(getTile());
        pos.offset(absDir);
        pos.offset(side);
        TileMultipart t = BasicUtils.getTileEntity(world(), pos, TileMultipart.class);
        if (t != null)
            return getPartStrength(t.partMap(absDir^1), Rotation.rotationTo(absDir^1, side^1));
        
        return 0;
    }
    
    @Override
    public int getStrength() {
        return strength & 0xFF;
    }
}
