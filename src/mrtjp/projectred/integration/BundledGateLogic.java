package mrtjp.projectred.integration;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.projectred.api.IBundledEmitter;
import mrtjp.projectred.api.IConnectable;
import net.minecraft.nbt.NBTTagCompound;

import java.util.Random;

import static mrtjp.projectred.transmission.BundledCommons.*;

public abstract class BundledGateLogic extends RedstoneGateLogic<BundledGatePart>
{
    public static BundledGateLogic create(BundledGatePart gate, int subID)
    {
        if (subID == 22)
            return new BusTransceiver(gate);
        if (subID == 28)
            return new BusRandomizer(gate);
        if (subID == 29)
            return new BusConverter(gate);
        return null;
    }

    public BundledGateLogic(BundledGatePart gate)
    {
        this.gate = gate;
    }

    public BundledGatePart gate;

    @Override
    public boolean canConnectTo(BundledGatePart gate, IConnectable wire, int r)
    {
        if (super.canConnectTo(gate, wire, r))
            return true;
        if (wire instanceof IBundledEmitter)
            return canConnectBundled(gate, r);
        return false;
    }

    public boolean canConnectBundled(BundledGatePart gate, int r)
    {
        return canConnectBundled(gate.shape(), r);
    }

    public boolean canConnectBundled(int shape, int r)
    {
        return ((bundledInputMask(shape)|bundledOutputMask(shape))&1<<r) != 0;
    }

    public int bundledInputMask(int shape)
    {
        return 0;
    }

    public int bundledOutputMask(int shape)
    {
        return 0;
    }

    @Override
    public int getOutput(BundledGatePart gate, int r)
    {
        return (gate.state&0x10<<r) != 0 ? 15 : 0;
    }

    public byte[] getBundledOutput(BundledGatePart gate, int r)
    {
        return null;
    }

    @Override
    public void onChange(BundledGatePart gate)
    {
    }

    @Override
    public void scheduledTick(BundledGatePart gate)
    {
    }

    public void save(NBTTagCompound tag)
    {
    }

    public void load(NBTTagCompound tag)
    {
    }

    public void readDesc(MCDataInput packet)
    {
    }

    public void writeDesc(MCDataOutput packet)
    {
    }

    public void read(MCDataInput packet, int switch_key)
    {
    }

    public static class BusTransceiver extends BundledGateLogic
    {
        public byte[] input0, output0, input2, output2;

        public BusTransceiver(BundledGatePart gate)
        {
            super(gate);
        }

        @Override
        public void save(NBTTagCompound tag)
        {
            saveSignal(tag, "in0", input0);
            saveSignal(tag, "out0", output0);
            saveSignal(tag, "in2", input2);
            saveSignal(tag, "out2", output2);
        }

        @Override
        public void load(NBTTagCompound tag)
        {
            input0 = loadSignal(tag, "in0");
            input2 = loadSignal(tag, "in2");
            output0 = loadSignal(tag, "out0");
            output2 = loadSignal(tag, "out2");
        }

        @Override
        public void readDesc(MCDataInput packet)
        {
            unpackClientData(packet.readInt());
        }

        @Override
        public void writeDesc(MCDataOutput packet)
        {
            packet.writeInt(packClientData());
        }

        @Override
        public void read(MCDataInput packet, int switch_key)
        {
            if (switch_key == 11)
                unpackClientData(packet.readInt());
        }

        public void sendClientUpdate()
        {
            gate.getWriteStream(11).writeInt(packClientData());
        }

        public int packClientData()
        {
            return packDigital(output0)|packDigital(output2)<<16;
        }

        public void unpackClientData(int packed)
        {
            output0 = unpackDigital(output0, packed&0xFFFF);
            output2 = unpackDigital(output2, packed>>>16);
        }

        @Override
        public int bundledInputMask(int shape)
        {
            return 1|4;
        }

        @Override
        public int bundledOutputMask(int shape)
        {
            return 1|4;
        }

        @Override
        public int inputMask(int shape)
        {
            return 2|8;
        }

        @Override
        public int outputMask(int shape)
        {
            return 0;
        }

        @Override
        public byte[] getBundledOutput(BundledGatePart gate, int r)
        {
            return r == 0 ? output0 : output2;
        }

        public byte[] getBundledInput(int r)
        {
            return raiseSignal(copySignal(gate.getBundledInput(r)), getBundledOutput(gate, r));// or'd with our output
        }

        @Override
        public void onChange(BundledGatePart gate)
        {
            boolean inputChanged = false;

            int oldInput = gate.state()&0xF;
            int newInput = getInput(gate, 2|8);
            if (oldInput != newInput)
            {
                gate.setState(gate.state()&0xF0|newInput);
                inputChanged = true;
            }

            byte[] newInput0 = getBundledInput(0);
            if (!signalsEqual(input0, newInput0))
            {
                input0 = newInput0;
                inputChanged = true;
            }

            byte[] newInput2 = getBundledInput(2);
            if (!signalsEqual(input2, newInput2))
            {
                input2 = newInput2;
                inputChanged = true;
            }

            if (inputChanged) gate.onInputChange();

            if (!signalsEqual(output0, getBundledOutput(0)) || !signalsEqual(output2, getBundledOutput(2)))
                gate.scheduleTick(2);
        }

        public byte[] getBundledOutput(int r)
        {
            int input = gate.state()&0xF;
            if (gate.shape() == 1)
                input = GatePart.flipMaskZ(input);

            if (r == 0)
                return (input&2) != 0 ? input2 : null;
            if (r == 2)
                return (input&8) != 0 ? input0 : null;

            return null;
        }

        @Override
        public void scheduledTick(BundledGatePart gate)
        {
            output0 = getBundledOutput(0);
            output2 = getBundledOutput(2);

            onChange(gate);
            gate.onOutputChange(1|4);
            sendClientUpdate();
        }

        @Override
        public boolean cycleShape(BundledGatePart gate)
        {
            gate.setShape(gate.shape() == 0 ? 1 : 0);
            return true;
        }

        @Override
        public int lightLevel()
        {
            return 0;
        }
    }

    public static class BusRandomizer extends BundledGateLogic
    {
        public Random rand = new Random();

        byte[] unpackedOut = new byte[16];
        int output = 0;
        int mask = 0xFFFF;

        public BusRandomizer(BundledGatePart gate)
        {
            super(gate);
        }

        @Override
        public int bundledOutputMask(int shape)
        {
            return 5;
        }

        @Override
        public int inputMask(int shape)
        {
            return 10;
        }

        @Override
        public int outputMask(int shape)
        {
            return 0;
        }

        @Override
        public void onChange(BundledGatePart gate)
        {
            boolean inputChanged = false;

            int oldInput = gate.state()&0xF;
            int newInput = getInput(gate, 10);
            if (oldInput != newInput)
            {
                gate.setState(gate.state()&0xF0|newInput);
                inputChanged = true;
            }

            int newMask = packDigital(gate.getBundledInput(2));
            if (newMask == 0) newMask = 0xFFFF;
            if (mask != newMask)
            {
                mask = newMask;
                inputChanged = true;
                sendMaskUpdate();
            }

            if (inputChanged) gate.onInputChange();

            if (newInput != 0) gate.scheduleTick(2);
        }

        @Override
        public void scheduledTick(BundledGatePart gate)
        {
            int oldOut = output;
            output = (gate.state()&0xF) != 0 ? gate.shape() == 0 ? calc1BitOut() : calcNBitOut() : oldOut;
            if (oldOut != output)
            {
                unpackedOut = unpackDigital(unpackedOut, output);
                gate.onOutputChange(1);
                sendOutUpdate();
            }
            onChange(gate);
        }

        private int calc1BitOut()
        {
            int high = Integer.bitCount(mask);
            int n = rand.nextInt(high);
            int v = 0;
            for (int i = 0; i < 16; i++) if ((mask&1<<i) != 0 && v++ == n) return 1<<i;
            return 0;
        }

        private int calcNBitOut()
        {
            int out = 0;
            for (int i = 0; i < 16; i++) if ((mask&1<<i) != 0 && rand.nextBoolean()) out |= 1<<i;
            return out;
        }

        @Override
        public void readDesc(MCDataInput packet)
        {
            output = packet.readUShort();
            mask = packet.readUShort();
        }

        @Override
        public void writeDesc(MCDataOutput packet)
        {
            packet.writeShort(output);
            packet.writeShort(mask);
        }

        @Override
        public void read(MCDataInput packet, int switch_key)
        {
            if (switch_key == 11)
                output = packet.readUShort();
            else if (switch_key == 12)
                mask = packet.readUShort();
        }

        private void sendOutUpdate()
        {
            gate.getWriteStream(11).writeShort(output);
        }

        private void sendMaskUpdate()
        {
            gate.getWriteStream(12).writeShort(mask);
        }

        @Override
        public void save(NBTTagCompound tag)
        {
            tag.setShort("in", (short)(mask&0xFFFF));
            tag.setShort("out", (short)(output&0xFFFF));
        }

        @Override
        public void load(NBTTagCompound tag)
        {
            mask = tag.getShort("in");
            output = tag.getShort("out");
            unpackedOut = unpackDigital(unpackedOut, output);
        }

        @Override
        public byte[] getBundledOutput(BundledGatePart gate, int r)
        {
            return r == 0 ? unpackedOut : null;
        }

        @Override
        public boolean cycleShape(BundledGatePart gate)
        {
            gate.setShape(gate.shape()^1);
            return true;
        }

        @Override
        public int lightLevel()
        {
            return 0;
        }
    }

    public static class BusConverter extends BundledGateLogic
    {
        public int bIn, bOut;
        public int rsIn, rsOut;
        public byte[] bOutUnpacked;

        public BusConverter(BundledGatePart gate)
        {
            super(gate);
        }

        public void setBOut(int newBOut)
        {
            if (bOut == newBOut) return;
            bOut = newBOut;
            bOutUnpacked = unpackDigital(bOutUnpacked, bOut);
        }

        @Override
        public int bundledOutputMask(int shape){ return shape == 0 ? 1 : 0; }

        @Override
        public int bundledInputMask(int shape){ return shape == 0 ? 0 : 1; }

        @Override
        public int outputMask(int shape){ return shape == 0 ? 10 : 14; }

        @Override
        public int inputMask(int shape){ return shape == 0 ? 4 : 0; }

        @Override
        public void save(NBTTagCompound tag)
        {
            tag.setByte("in", (byte)rsIn);
            tag.setByte("out", (byte)rsOut);
            tag.setByte("in0", (byte)bIn);
            tag.setByte("out0", (byte)bOut);
        }

        @Override
        public void load(NBTTagCompound tag)
        {
            rsIn = tag.getByte("in");
            rsOut = tag.getByte("out");
            bIn = tag.getByte("in0");
            setBOut(tag.getByte("out0"));
        }

        @Override
        public void onChange(BundledGatePart gate)
        {
            if (gate.shape() == 0)
            {
                int oldInput = rsIn;
                rsIn = gate.getRedstoneInput(2)/17;
                if (oldInput != rsIn)
                {
                    gate.onInputChange();
                    gate.scheduleTick(2);
                }
            }
            else
            {
                int oldInput = bIn;
                bIn = packDigital(gate.getBundledInput(0));
                if (oldInput != bIn)
                {
                    gate.onInputChange();
                    gate.scheduleTick(2);
                }
            }
        }

        @Override
        public void scheduledTick(BundledGatePart gate)
        {
            int changeMask = 0;

            if (gate.shape() == 0)
            {
                int oldOut = bOut;
                setBOut(1<<rsIn);
                if (oldOut != bOut) changeMask = 1;
            }
            else
            {
                int oldOut = rsOut;
                rsOut = mostSignificantBit(bIn);
                if (rsOut != oldOut) changeMask = 4;
            }

            int oldOut2 = gate.state()>>4;
            int newOut2 = (gate.shape() == 0 ? rsIn : bIn) != 0 ? 10 : 0;
            if (oldOut2 != newOut2)
            {
                gate.setState(gate.state()&0xF|newOut2<<4);
                changeMask |= 10;
            }

            if (changeMask != 0) gate.onOutputChange(changeMask);
            onChange(gate);
        }

        @Override
        public int getOutput(BundledGatePart gate, int r)
        {
            return gate.shape() == 1 && r == 2 ? rsOut : (gate.state()&0x10<<r) != 0 ? 15 : 0;
        }

        @Override
        public byte[] getBundledOutput(BundledGatePart gate, int r)
        {
            return gate.shape() == 0 && r == 0 ? bOutUnpacked : null;
        }

        /**
         * Shape 0: Analog to Digital (RS to Bundled)
         * Shape 1: Digital to Analog (Bundled to RS)
         */
        @Override
        public boolean cycleShape(BundledGatePart gate)
        {
            gate.setShape(gate.shape()^1);
            return true;
        }

        @Override
        public int lightLevel()
        {
            return 0;
        }

        public int packBData(){ return bIn<<16|bOut; }
        public void unpackBData(int pack)
        {
            bIn = pack>>16;
            bOut = pack&0xFFFF;
        }

        public int packData(){ return rsIn<<4|rsOut; }
        public void unpackData(int pack)
        {
            rsIn = pack>>4;
            rsOut = pack&0xF;
        }

        public void sendBUpdate()
        {
            gate.getWriteStream(11).writeInt(packBData());
        }

        public void sendUpdate()
        {
            gate.getWriteStream(12).writeShort(packData());
        }

        @Override
        public void readDesc(MCDataInput packet)
        {
            unpackBData(packet.readInt());
            unpackData(packet.readUShort());
        }

        @Override
        public void writeDesc(MCDataOutput packet)
        {
            packet.writeInt(packBData()).writeShort(packData());
        }

        @Override
        public void read(MCDataInput packet, int key)
        {
            if (key == 11) unpackBData(packet.readInt());
            else if (key == 12) unpackData(packet.readUShort());
        }
    }
}
