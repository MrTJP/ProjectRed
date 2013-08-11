package mrtjp.projectred.transmission;

import mrtjp.projectred.core.BasicUtils;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.ChatMessageComponent;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Rotation;
import codechicken.multipart.IFaceRedstonePart;
import codechicken.multipart.IRedstonePart;
import codechicken.multipart.RedstoneInteractions;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;

public abstract class RedwirePart extends WirePart implements IRedstoneEmitter, IFaceRedstonePart {

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
        return (strength&0xFF)>>4;
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
    
    @Override
    protected void updatePowerState(boolean force) {
        int newStrength = calculateStrength();
        if(newStrength != strength || force) {
            strength = (byte)newStrength;
            updateConnected();
        }
    }
    
    public void updateConnected() {
        //TODO push notifications
    }

    public int calculateStrength() {
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
        
        return s;
    }

    public int calculateUndersideStrength() {
        return 0;
    }

    public int calculateInternalStrength(int r) {
        int absDir = Rotation.rotateSide(side, r);
        
        TMultiPart tp = tile().partMap(absDir);
        if(tp instanceof IRedstoneEmitter) {
            return ((IRedstoneEmitter) tp).getEmittedSignalStrength(Rotation.rotationTo(absDir, side))-1;
        }
        else if(tp instanceof IRedstonePart) {
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
            TMultiPart tp = t.partMap(side);
            if (tp instanceof IRedstoneEmitter)
                return ((IRedstoneEmitter) tp).getEmittedSignalStrength((r+2)%4)-1;
        }
        //TODO wires
        return RedstoneInteractions.getPowerTo(this, absDir) << 4;
    }

    public int calculateCornerStrength(int r) {
        int absDir = Rotation.rotateSide(side, r);
        
        BlockCoord pos = new BlockCoord(getTile());
        pos.offset(absDir);
        pos.offset(side);
        TileMultipart t = BasicUtils.getTileEntity(world(), pos, TileMultipart.class);
        if (t != null) {
            TMultiPart tp = t.partMap(absDir^1);
            if (tp instanceof IRedstoneEmitter)
                return ((IRedstoneEmitter) tp).getEmittedSignalStrength(Rotation.rotationTo(absDir^1, side^1))-1;
        }
        
        return 0;
    }
    
    @Override
    public short getEmittedSignalStrength(int side) {
        return strength;
    }
}
