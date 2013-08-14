package mrtjp.projectred.transmission;

import java.util.Arrays;

import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.CoreCPH;
import mrtjp.projectred.core.Messenger;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.ChatMessageComponent;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Rotation;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;

public class BundledCablePart extends WirePart implements IBundledCablePart, IBundledEmitter {
    public static byte[] tmpSignal = new byte[16];

    public static boolean signalsEqual(byte[] signal1, byte[] signal2) {
        for (int i = 0; i < 16; i++)
            if (signal1[i] != signal2[i])
                return false;

        return true;
    }

    public static boolean isSignalZero(byte[] signal) {
        if (signal == null)
            return true;

        for (int i = 0; i < 16; i++)
            if (signal[i] != 0)
                return false;

        return true;
    }

    public static boolean dropSignalsLessThan(byte[] signal1, byte[] signal2) {
        boolean dropped = false;

        for (int i = 0; i < 16; i++)
            if ((signal2[i] & 0xFF) < (signal1[i] & 0xFF)) {
                signal1[i] = 0;
                dropped = true;
            }

        return dropped;
    }

    /**
     * Not available on client
     */
    public byte[] signal = new byte[16];
    public byte colour;

    public BundledCablePart(int side) {
        super(side);
    }

    @Override
    public String getType() {
        return "pr_bundled";
    }

    @Override
    public EnumWire getWireType() {
        return EnumWire.BUNDLED_WIRE[colour + 1];
    }

    @Override
    public void onPlaced(int side, int meta) {
        super.onPlaced(side, meta);
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
    public boolean canConnectToType(WirePart wire, int r) {
        if (wire instanceof BundledCablePart) {
            int ocolour = ((BundledCablePart) wire).colour;
            return ocolour == -1 || colour == -1 || ocolour == colour;
        } else if (wire instanceof InsulatedRedAlloyPart) {
            return true;
        }

        return false;
    }

    @Override
    public void updateAndPropogate(TMultiPart prev, int mode) {
        if (mode == DROPPING && isSignalZero(signal))
            return;

        byte[] newSignal = calculateSignal();
        if (dropSignalsLessThan(signal, newSignal)) {
            propogate(prev, DROPPING);
        } else if (!signalsEqual(signal, newSignal)) {
            setSignal(newSignal);
            if (mode == DROPPING)
                propogate(null, RISING);
            else
                propogate(prev, RISING);
        } else {
            if (mode == DROPPING)
                propogateTo(prev, RISING);
            else if (mode == FORCE)
                propogate(prev, FORCED);
        }
    }

    @Override
    public boolean propogateTo(TMultiPart part, int mode) {
        if (mode == DROPPING && part instanceof InsulatedRedAlloyPart)
            if (signal[((InsulatedRedAlloyPart) part).colour] != 0)// no point
                                                                   // propogating
                                                                   // to an ins
                                                                   // wire if we
                                                                   // didn't
                                                                   // drop their
                                                                   // colour
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

        BlockCoord pos = new BlockCoord(getTile()).offset(absDir).offset(side);
        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if (t != null)
            calculatePartSignal(t.partMap(absDir ^ 1), Rotation.rotationTo(absDir ^ 1, side ^ 1));
    }

    public void calculateStraightSignal(int r) {
        int absDir = Rotation.rotateSide(side, r);

        BlockCoord pos = new BlockCoord(getTile()).offset(absDir);
        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if (t != null)
            calculatePartSignal(t.partMap(side), (r + 2) % 4);
    }

    public void calculateInternalSignal(int r) {
        int absDir = Rotation.rotateSide(side, r);

        calculatePartSignal(tile().partMap(absDir), (r + 2) % 4);
    }

    public void calculateCenterSignal() {
        calculatePartSignal(tile().partMap(6), side);
    }

    public void calculatePartSignal(TMultiPart part, int r) {
        if (part instanceof IBundledCablePart) {
            byte[] osignal = ((IBundledCablePart) part).getBundledSignal();
            for (int i = 0; i < 16; i++)
                if ((osignal[i] & 0xFF) - 1 > (tmpSignal[i] & 0xFF))
                    tmpSignal[i] = (byte) (osignal[i] - 1);
        } else if (part instanceof InsulatedRedAlloyPart) {
            InsulatedRedAlloyPart insPart = (InsulatedRedAlloyPart) part;
            int s = insPart.getRedwireSignal() - 1;
            if (s > (tmpSignal[insPart.colour] & 0xFF))
                tmpSignal[insPart.colour] = (byte) s;
        } else if (part instanceof IBundledEmitter) {
            byte[] osignal = ((IBundledEmitter) part).getBundledSignal(r);
            if (osignal != null) {
                for (int i = 0; i < 16; i++)
                    if ((osignal[i] & 0xFF) > (tmpSignal[i] & 0xFF))
                        tmpSignal[i] = osignal[i];
            }
        }
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

        ply.sendChatToPlayer(ChatMessageComponent.func_111077_e(sb.toString()));
        return true;
    }

    @Override
    protected boolean test(EntityPlayer player) {
        if (BasicUtils.isServer(world())) {
            String s = "";
            for (int i = 0; i < 16; i++) {
                int x = getBundledSignal()[i];
                if (x != 0) {
                    s = s + "[" + i + "]";
                }
            }
            if (s == "") {
                s = "off";
            }
            PacketCustom packet = new PacketCustom(Configurator.corePacketChannel, CoreCPH.messengerQueue);
            packet.writeFloat(x() + 0.0f);
            packet.writeFloat(y() + 0.5f);
            packet.writeFloat(z() + 0.0f);
            packet.writeString(s);
            packet.sendToPlayer(player);
        }
        return true;
    }
}
