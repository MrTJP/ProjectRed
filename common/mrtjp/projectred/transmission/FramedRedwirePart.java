package mrtjp.projectred.transmission;

import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.CoreProxy;
import mrtjp.projectred.core.Messenger;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.MovingObjectPosition;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.IMaskedRedstonePart;
import codechicken.multipart.IRedstonePart;
import codechicken.multipart.RedstoneInteractions;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;

public abstract class FramedRedwirePart extends FramedWirePart implements IRedwirePart, IMaskedRedstonePart
{
    public byte signal;
    
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
        if(switch_key == 10) {
            signal = packet.readByte();
            tile().markRender();
        }
        else
            super.read(packet, switch_key);
    }
    
    @Override
    public int strongPowerLevel(int side) {
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
        if(WirePropogator.redwiresProvidePower)
            return ((signal&0xFF)+16)/17;
        
        return 0;
    }
    
    @Override
    public int getConnectionMask(int side) {
        return 0x10;
    }
    
    @Override
    public boolean canConnectToType(IConnectable wire) {
        return wire instanceof IRedwirePart;
    }

    @Override
    public boolean connectStraightOverride(int absDir) {
        WirePropogator.setRedwiresConnectable(false);
        boolean b = (RedstoneInteractions.otherConnectionMask(world(), x(), y(), z(), absDir, false) & 
                RedstoneInteractions.connectionMask(this, absDir)) != 0;
        WirePropogator.setRedwiresConnectable(true);
        return b;
    }
    
    @Override
    public boolean connectInternalOverride(TMultiPart p, int s) {
        if (p instanceof IRedstonePart) {
            IRedstonePart rsPart = (IRedstonePart)p;
            return rsPart.canConnectRedstone(s^1);
        }
        
        return false;
    }
    
    @Override
    public void updateAndPropogate(TMultiPart prev, int mode) {
        if(mode == DROPPING && signal == 0)
            return;
        
        int newSignal = calculateSignal();
        if(newSignal < getRedwireSignal()) {
            signal = 0;
            propogate(prev, DROPPING);
        }
        else if(newSignal > getRedwireSignal()) {
            signal = (byte)newSignal;
            if(mode == DROPPING)
                propogate(null, RISING);
            else
                propogate(prev, RISING);
        }
        else {
            if(mode == DROPPING)
                propogateTo(prev, RISING, Integer.MAX_VALUE);
            else if(mode == FORCE)
                propogate(prev, FORCED);
        }
    }

    public void onSignalUpdate() {
        super.onSignalUpdate();
        tile().getWriteStream(this).writeByte(10).writeByte(signal);
    }
    
    public int calculateSignal() {
        WirePropogator.setWiresProvidePower(false);
        WirePropogator.redwiresProvidePower = false;
        
        int str = 0;
        for(int s = 0; s < 6; s++)
            if(maskConnects(s)) {
                int i;
                if((connMap & 1<<s) != 0)
                    i = calculateStraightSignal(s);
                else
                    i = calculateInternalSignal(s);
                
                if(i > str)
                    str = i;
            }
        
        WirePropogator.setWiresProvidePower(true);
        WirePropogator.redwiresProvidePower = true;
        
        return str;
    }

    public int calculateStraightSignal(int s) {
        BlockCoord pos = new BlockCoord(getTile()).offset(s);
        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if (t != null) {
            TMultiPart tp = t.partMap(6);
            if(tp != null)
                return getPartSignal(tp, s^1);
        }
    
        return RedstoneInteractions.getPowerTo(this, s)*17;
    }

    public int calculateInternalSignal(int s) {
        TMultiPart tp = tile().partMap(s);
        int i = getPartSignal(tp, -1);
        if(i > 0)
            return i;
        
        if(tp instanceof IRedstonePart) {
            IRedstonePart rp = (IRedstonePart) tp;
            return Math.max(rp.strongPowerLevel(s^1), rp.weakPowerLevel(s^1)) << 4;
        }
        
        return 0;
    }

    public int getPartSignal(TMultiPart part, int r) {
        if(part instanceof IRedwirePart && ((IRedwirePart) part).isWireSide(r))
            return ((IRedwirePart) part).getRedwireSignal(r) - 1;
        else if(part instanceof IRedwireEmitter)
            return ((IRedwireEmitter) part).getRedwireSignal(r);
        
        return 0;
    }
    
    public int getRedwireSignal() {
        return signal & 0xFF;
    }
    
    @Override
    public int getRedwireSignal(int side) {
        return getRedwireSignal();
    }
    
    @Override
    protected boolean test(EntityPlayer player, MovingObjectPosition hit) {
        if (BasicUtils.isClient(world())) {
            Messenger.addMessage(x() + 0, y() + .5f, z() + 0,  "/#f/#c[c] = " + getRedwireSignal());
        } else {
            PacketCustom packet = new PacketCustom(Configurator.corePacketChannel, CoreProxy.messengerQueue);
            packet.writeDouble(x() + 0.0D);
            packet.writeDouble(y() + 0.5D);
            packet.writeDouble(z() + 0.0D);
            packet.writeString("/#c[s] = " + getRedwireSignal());
            packet.sendToPlayer(player);
        }
        return true;
    }

}
