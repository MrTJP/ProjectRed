package mrtjp.projectred.transmission;

import static mrtjp.projectred.transmission.BundledCableCommons.calculatePartSignal;
import static mrtjp.projectred.transmission.BundledCableCommons.tmpSignal;

import java.util.Arrays;

import mrtjp.projectred.api.IBundledEmitter;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.CoreSPH;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.ChatMessageComponent;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Rotation;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;

public class BundledCablePart extends WirePart implements IBundledCablePart {

    /**
     * Not available on client
     */
    public byte[] signal = new byte[16];
    public byte colour;

    @Override
    public String getType() {
        return "pr_bundled";
    }

    @Override
    public EnumWire getWireType() {
        return EnumWire.BUNDLED_WIRE[colour + 1];
    }

    @Override
    public void preparePlacement(int side, int meta) {
        super.preparePlacement(side, meta);
        colour = (byte) (meta - EnumWire.BUNDLED_0.ordinal());
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
    public boolean canConnectToType(IConnectable wire) {
        if (wire instanceof BundledCablePart) {
            int ocolour = ((BundledCablePart) wire).colour;
            return ocolour == -1 || colour == -1 || ocolour == colour;
        } else if (wire instanceof IInsulatedRedwirePart || wire instanceof IBundledEmitter) {
            return true;
        }

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

        for (int r = 0; r < 4; r++)
            if (maskConnects(r)) {
                if ((connMap & 1 << r) != 0)
                    calculateCornerSignal(r);
                else if ((connMap & 0x10 << r) != 0)
                    calculateStraightSignal(r);
                else
                    calculateInternalSignal(r);
            }

        calculateCenterSignal();

        return tmpSignal;
    }

    public void calculateCornerSignal(int r) {
        int absDir = Rotation.rotateSide(side, r);

        BlockCoord pos = new BlockCoord(tile()).offset(absDir).offset(side);
        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if (t != null)
            calculatePartSignal(t.partMap(absDir ^ 1), Rotation.rotationTo(absDir ^ 1, side ^ 1));
    }

    public void calculateStraightSignal(int r) {
        int absDir = Rotation.rotateSide(side, r);

        BlockCoord pos = new BlockCoord(tile()).offset(absDir);
        TileEntity t = world().getBlockTileEntity(pos.x, pos.y, pos.z);
        if(t instanceof IBundledEmitter)
            calculatePartSignal(t, absDir^1);
        else if(t instanceof TileMultipart)
            calculatePartSignal(((TileMultipart)t).partMap(side), (r + 2) % 4);
    }

    public void calculateInternalSignal(int r) {
        int absDir = Rotation.rotateSide(side, r);

        calculatePartSignal(tile().partMap(absDir), (r + 2) % 4);
    }

    public void calculateCenterSignal() {
        calculatePartSignal(tile().partMap(6), side);
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
    protected boolean debug(EntityPlayer ply) {
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < 16; i++) {
            String s = Integer.toHexString(signal[i] & 0xFF).toUpperCase();
            if (s.length() == 1)
                sb.append('0');
            sb.append(s);
        }

        ply.sendChatToPlayer(ChatMessageComponent.createFromTranslationKey(sb.toString()));
        return true;
    }

    @Override
    protected boolean test(EntityPlayer player) {
        if (BasicUtils.isServer(world())) {
            String s = "";
            for (int i = 0; i < 16; i++) {
                int x = getBundledSignal()[i];
                if (x != 0)
                    s = s + "[" + i + "]";
            }
            if (s == "")
                s = "off";

            PacketCustom packet = new PacketCustom(CoreSPH.channel, 2);
            packet.writeDouble(x() + 0.0D);
            packet.writeDouble(y() + 0.5D);
            packet.writeDouble(z() + 0.0D);
            packet.writeString("/#f" + s);
            packet.sendToPlayer(player);
        }
        return true;
    }
}