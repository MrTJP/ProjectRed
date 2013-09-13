package mrtjp.projectred.transmission;

import static mrtjp.projectred.transmission.BundledCableCommons.calculatePartSignal;
import static mrtjp.projectred.transmission.BundledCableCommons.tmpSignal;

import java.util.Arrays;

import mrtjp.projectred.api.IBundledEmitter;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.CoreCPH;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.MovingObjectPosition;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;

public class FramedBundledCablePart extends FramedWirePart implements IBundledCablePart
{
    /**
     * Not available on client
     */
    public byte[] signal = new byte[16];
    
    @Override
    public String getType() {
        return "pr_sbundled";
    }

    @Override
    public EnumWire getWireType() {
        return EnumWire.BUNDLED_N;
    }

    @Override
    public void save(NBTTagCompound tag) {
        super.save(tag);
        tag.setByteArray("signal", signal);
    }

    @Override
    public void load(NBTTagCompound tag) {
        super.load(tag);
        signal = tag.getByteArray("signal");
    }

    @Override
    public boolean canConnectToType(IConnectable wire) {
        if (wire instanceof IInsulatedRedwirePart || wire instanceof IBundledEmitter)
            return true;
        
        return false;
    }

    @Override
    public void updateAndPropogate(TMultiPart prev, int mode) {
        BundledCableCommons.updateAndPropogate(this, prev, mode);
    }

    @Override
    public boolean propogateTo(TMultiPart part, int mode) {
        if(!BundledCableCommons.shouldPropogate(this, part, mode))
            return true;

        return super.propogateTo(part, mode);
    }

    public void setSignal(byte[] newSignal) {
        if (newSignal == null)
            Arrays.fill(signal, (byte) 0);
        else
            System.arraycopy(newSignal, 0, signal, 0, 16);
    }

    public byte[] calculateSignal() {
        Arrays.fill(tmpSignal, (byte) 0);

        for(int s = 0; s < 6; s++)
            if (maskConnects(s)) {
                if((connMap & 1<<s) != 0)
                    calculateStraightSignal(s);
                else
                    calculateInternalSignal(s);
            }
        
        return tmpSignal;
    }
    
    public void calculateStraightSignal(int s) {
        BlockCoord pos = new BlockCoord(getTile()).offset(s);
        TileEntity t = world().getBlockTileEntity(pos.x, pos.y, pos.z);
        if(t instanceof IBundledEmitter)
            calculatePartSignal(t, s^1);
        else if(t instanceof TileMultipart)
            calculatePartSignal(((TileMultipart)t).partMap(6), s^1);
    }

    public void calculateInternalSignal(int s) {
        calculatePartSignal(tile().partMap(s), -1);
    }
    
    @Override
    public byte[] getBundledSignal() {
        return signal;
    }

    @Override
    public byte[] getBundledSignal(int side) {
        return getBundledSignal();
    }

    @Override
    protected boolean test(EntityPlayer player, MovingObjectPosition hit) {
        if (BasicUtils.isServer(world())) {
            String s = "";
            for (int i = 0; i < 16; i++) {
                int x = getBundledSignal()[i];
                if (x != 0)
                    s = s + "[" + i + "]";
            }
            if (s == "")
                s = "off";

            PacketCustom packet = new PacketCustom(CoreCPH.channel, CoreCPH.messengerQueue);
            packet.writeDouble(x() + 0.0D);
            packet.writeDouble(y() + 0.5D);
            packet.writeDouble(z() + 0.0D);
            packet.writeString("/#f" + s);
            packet.sendToPlayer(player);
        }
        return true;
    }
}
