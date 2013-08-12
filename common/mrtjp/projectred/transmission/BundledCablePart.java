package mrtjp.projectred.transmission;

import java.util.Arrays;

import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.ChatMessageComponent;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Rotation;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.transmission.EnumWire;
import mrtjp.projectred.transmission.IBundledEmitter;
import mrtjp.projectred.transmission.InsulatedRedAlloyPart;
import mrtjp.projectred.transmission.WirePart;

public class BundledCablePart extends WirePart implements IBundledCablePart, IBundledEmitter
{
    public static byte[] tmpSignal = new byte[16];
    
    /**
     * Currently not available on client
     */
    public byte[] signal = new byte[16];
    public byte colour;
    
    public BundledCablePart(int side) {
        super(side);
    }
    
    @Override
    public void save(NBTTagCompound tag) {
        super.save(tag);
        tag.setByteArray("signal", signal);
        tag.setByte("colour", colour);
    }
    
    @Override
    public void load(NBTTagCompound tag) {
        super.load(tag);
        signal = tag.getByteArray("signal");
        colour = tag.getByte("colour");
    }
    
    @Override
    public void writeDesc(MCDataOutput packet) {
        super.writeDesc(packet);
        packet.writeByte(colour);
    }
    
    @Override
    public void readDesc(MCDataInput packet) {
        super.readDesc(packet);
        colour = packet.readByte();
    }
    
    @Override
    public void onPlaced(int side, int meta) {
        super.onPlaced(side, meta);
        colour = (byte)(meta-EnumWire.BUNDLED_0.ordinal());
    }
    
    @Override
    public EnumWire getWireType() {
        return EnumWire.BUNDLED_WIRE[colour+1];
    }
    
    @Override
    public String getType() {
        return "pr_bundled";
    }
    
    @Override
    public boolean canConnectToType(WirePart wire, int r) {
        if(wire instanceof BundledCablePart) {
            int ocolour = ((BundledCablePart) wire).colour;
            return ocolour == -1 || colour == -1 || ocolour == colour;
        }
        else if(wire instanceof InsulatedRedAlloyPart) {
            return true;
        }
        
        return false;
    }
    
    public void updateAndPropogate(TMultiPart prev, boolean force) {
        byte[] newSignal = calculateSignal();
        boolean changed = !signalsEqual(signal, newSignal);
        setSignal(newSignal);
        
        if(changed || force)
            propogate(prev);
    }

    public static boolean signalsEqual(byte[] signal1, byte[] signal2) {
        for(int i = 0; i < 16; i++)
            if(signal1[i] != signal2[i])
                return false;
        
        return true;
    }
    
    public void setSignal(byte[] newSignal) {
        System.arraycopy(newSignal, 0, signal, 0, 16);
    }
    
    public byte[] calculateSignal() {
        Arrays.fill(tmpSignal, (byte)0);
        
        for(int r = 0; r < 4; r++)
            if(maskConnects(r)) {
                if((connMap & 1<<r) != 0)
                    calculateCornerSignal(r);
                else if((connMap & 0x10<<r) != 0)
                    calculateStraightSignal(r);
                else
                    calculateInternalSignal(r);
            }
        
        calculateCenterSignal();
        
        return tmpSignal;
    }
    
    public void calculateCenterSignal() {
        calculatePartSignal(tile().partMap(6), -1);
    }

    public void calculateInternalSignal(int r) {
        int absDir = Rotation.rotateSide(side, r);
        
        calculatePartSignal(tile().partMap(absDir), (r+2)%4);
    }

    public void calculateStraightSignal(int r) {
        int absDir = Rotation.rotateSide(side, r);
        
        BlockCoord pos = new BlockCoord(getTile()).offset(absDir);
        TileMultipart t = BasicUtils.getTileEntity(world(), pos, TileMultipart.class);
        if (t != null)
            calculatePartSignal(t.partMap(side), (r+2)%4);
    }

    public void calculateCornerSignal(int r) {
        int absDir = Rotation.rotateSide(side, r);
        
        BlockCoord pos = new BlockCoord(getTile()) .offset(absDir).offset(side);
        TileMultipart t = BasicUtils.getTileEntity(world(), pos, TileMultipart.class);
        if (t != null)
            calculatePartSignal(t.partMap(absDir^1), Rotation.rotationTo(absDir^1, side^1));
    }
    
    public void calculatePartSignal(TMultiPart part, int r) {
        if(part instanceof IBundledCablePart) {
            byte[] osignal = ((IBundledCablePart) part).getSignal();
            for(int i = 0; i < 16; i++)
                if((osignal[i]&0xFF)-1 > (tmpSignal[i]&0xFF))
                    tmpSignal[i] = (byte) (osignal[i]-1);
        } else if(part instanceof InsulatedRedAlloyPart) {
            InsulatedRedAlloyPart insPart = (InsulatedRedAlloyPart)part;
            int s = insPart.getStrength()-1;
            if(s > (tmpSignal[insPart.colour]&0xFF))
                tmpSignal[insPart.colour] = (byte)s;
        }
        else if(part instanceof IBundledEmitter) {
            byte[] osignal = ((IBundledEmitter) part).getBundledCableStrength(r);
            if(osignal != null) {
                for(int i = 0; i < 16; i++)
                    if((osignal[i]&0xFF) > (tmpSignal[i]&0xFF))
                        tmpSignal[i] = osignal[i];
            }
        }
    }
    
    @Override
    protected boolean debug(EntityPlayer ply) {
        StringBuffer sb = new StringBuffer();
        for(int i = 0; i < 16; i++) {
            String s = Integer.toHexString(signal[i]&0xFF).toUpperCase();
            if(s.length() == 1)
                sb.append('0');
            sb.append(s);
        }
        
        ply.sendChatToPlayer(ChatMessageComponent.func_111077_e(sb.toString()));
        return true;
    }
    
    @Override
    public byte[] getSignal() {
        return signal;
    }
    
    @Override
    public byte[] getBundledCableStrength(int side) {
        return getSignal();
    }
}
