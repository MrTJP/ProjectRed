package mrtjp.projectred.transmission;

import java.util.Arrays;

import mrtjp.projectred.core.BasicUtils;
import net.minecraft.nbt.NBTTagCompound;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;

import static mrtjp.projectred.transmission.BundledCableCommons.*;

public class ScaffoldBundledCablePart extends ScaffoldWirePart implements IBundledCablePart, IBundledEmitter
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
    public boolean canConnectToType(IWirePart wire) {
        if (wire instanceof IInsulatedRedwirePart || wire instanceof IBundledCablePart)
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
        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if (t != null)
            calculatePartSignal(t.partMap(6), s^1);
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
}
