package mrtjp.projectred.integration;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.raytracer.ExtendedMOP;
import codechicken.lib.raytracer.IndexedCuboid6;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Vector3;
import mrtjp.projectred.api.IBundledEmitter;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.api.IScrewdriver;
import mrtjp.projectred.core.libmc.VecLib;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.MovingObjectPosition;

import java.util.List;
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
        if (subID == 30)
            return new BusInputPanel(gate);
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
            boolean changed = false;

            int oldRSIn = rsIn;
            rsIn = gate.shape() == 0 ? gate.getRedstoneInput(2)/17 : 0;
            if (oldRSIn != rsIn) changed = true;

            int oldBIn = bIn;
            bIn = gate.shape() == 0 ? 0 : packDigital(gate.getBundledInput(0));
            if (oldBIn != bIn) changed = true;

            if (changed)
            {
                gate.onInputChange();
                gate.scheduleTick(2);
                sendClientUpdate();
            }
        }

        @Override
        public void scheduledTick(BundledGatePart gate)
        {
            int changeMask = 0;

            int oldBOut = bOut;
            setBOut(gate.shape() == 0 ? rsIn > 0 ? 1<<rsIn : 0 : 0);
            if (oldBOut != bOut) changeMask |= 1;

            int oldRSOut = rsOut;
            rsOut = gate.shape() == 0 ? 0 : mostSignificantBit(bIn);
            if (rsOut != oldRSOut) changeMask |= 4;

            int oldOut2 = gate.state()>>4;
            int newOut2 = (gate.shape() == 0 ? rsIn : bIn) != 0 ? 10 : 0;
            if (oldOut2 != newOut2)
            {
                gate.setState(gate.state()&0xF|newOut2<<4);
                changeMask |= 10;
            }

            if (changeMask != 0)
            {
                gate.onOutputChange(changeMask);
                sendClientUpdate();
            }
            onChange(gate);
        }

        @Override
        public int getOutput(BundledGatePart gate, int r)
        {
            return gate.shape() != 0 && r == 2 ? rsOut : (gate.state()&0x10<<r) != 0 ? 15 : 0;
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

        public int packClientData()
        {
            //bbbb BBBB rrrr RRRR
            //R - rsIn
            //r - rsOut
            //B - bundled in
            //b - bunbled out
            return rsIn|rsOut<<4|mostSignificantBit(bIn)<<8|mostSignificantBit(bOut)<<12;
        }

        public void unpackClientData(int data)
        {
            rsIn = data&0xF;
            rsOut = data>>4&0xF;
            bIn = 1<<(data>>8&0xF);
            setBOut(1<<(data>>12&0xF));
        }

        public void sendClientUpdate()
        {
            gate.getWriteStream(11).writeShort(packClientData());
        }

        @Override
        public void readDesc(MCDataInput packet)
        {
            unpackClientData(packet.readUShort());
        }

        @Override
        public void writeDesc(MCDataOutput packet)
        {
            packet.writeShort(packClientData());
        }

        @Override
        public void read(MCDataInput packet, int key)
        {
            if (key == 11) unpackClientData(packet.readUShort());
        }
    }

    public static class BusInputPanel extends BundledGateLogic
    {
        public static IndexedCuboid6[] unpressed;
        public static IndexedCuboid6[] pressed;

        static
        {
            unpressed = VecLib.buildCubeArray(4, 4, new Cuboid6(3, 1, 3, 13, 3, 13), new Vector3(-0.25, 0, -0.25));
            pressed = VecLib.buildCubeArray(4, 4, new Cuboid6(3, 1, 3, 13, 2.5, 13), new Vector3(-0.25, 0, -0.25));
        }

        public int pressMask = 0;

        public int bOut = 0;
        public byte[] bOutUnpack;

        public void setBOut(int newBOut)
        {
            if (bOut == newBOut) return;
            bOut = (short)newBOut;
            bOutUnpack = unpackDigital(bOutUnpack, bOut);
        }

        public BusInputPanel(BundledGatePart gate)
        {
            super(gate);
        }

        @Override
        public void addSubParts(BundledGatePart part, List<IndexedCuboid6> list)
        {
            for (int i = 0; i < 16; i++)
            {
                IndexedCuboid6[] array = (pressMask&1<<i) == 0 ? unpressed : pressed;
                list.add(new IndexedCuboid6(i, array[i].copy().apply(VecLib.orientT(part.orientation))));
            }
        }

        @Override
        public boolean activate(BundledGatePart part, EntityPlayer player, ItemStack held, MovingObjectPosition hit)
        {
            if (held != null && held.getItem() instanceof IScrewdriver) return false;
            ExtendedMOP hit1 = (ExtendedMOP)hit;
            int hitdata = ((Integer)hit1.data);
            if (hitdata != -1)
            {
                pressMask ^= (1<<hitdata);
                if (!part.world().isRemote) onChange(part);
                return true;
            }
            return false;
        }

        @Override
        public int bundledOutputMask(int shape)
        {
            return 4;
        }

        @Override
        public int bundledInputMask(int shape)
        {
            return 0;
        }

        @Override
        public int outputMask(int shape)
        {
            return 0;
        }

        @Override
        public int inputMask(int shape)
        {
            return 1;
        }

        @Override
        public void save(NBTTagCompound tag)
        {
            tag.setShort("press", (short)pressMask);
            tag.setShort("mask", (short)bOut);
        }

        @Override
        public void load(NBTTagCompound tag)
        {
            pressMask = tag.getShort("press");
            setBOut(tag.getShort("mask"));
        }

        @Override
        public void onChange(BundledGatePart gate)
        {
            boolean inputChanged = false;

            int oldInput = gate.state()&0xF;
            int newInput = getInput(gate, 1);
            if (oldInput != newInput)
            {
                gate.setState(gate.state()&0xF0|newInput);
                inputChanged = true;
            }

            if ((gate.state()&1) != 0) pressMask = 0;

            int oldBInput = bOut;
            int newBInput = pressMask;
            if (oldBInput != newBInput)
            {
                inputChanged = true;
            }

            if (inputChanged)
            {
                gate.onInputChange();
                gate.scheduleTick(2);
            }
        }

        @Override
        public void scheduledTick(BundledGatePart gate)
        {
            boolean outputChanged = false;

            int oldBOut = bOut;
            int newBOut = pressMask;
            if (oldBOut != newBOut)
            {
                setBOut(pressMask);
                outputChanged = true;
                sendClientUpdate();
            }

            if (outputChanged)
                gate.onOutputChange(bundledOutputMask(gate.shape()));

            onChange(gate);
        }

        @Override
        public int getOutput(BundledGatePart gate, int r)
        {
            return (gate.state()&0x10<<r) != 0 ? 15 : 0;
        }

        @Override
        public byte[] getBundledOutput(BundledGatePart gate, int r)
        {
            return bOutUnpack;
        }

        public void sendClientUpdate()
        {
            gate.getWriteStream(11).writeShort(pressMask);
        }

        @Override
        public void read(MCDataInput packet, int key)
        {
            if (key == 11) pressMask = packet.readUShort();
        }

        @Override
        public void readDesc(MCDataInput packet)
        {
            pressMask = packet.readUShort();
        }

        @Override
        public void writeDesc(MCDataOutput packet)
        {
            packet.writeShort(pressMask);
        }
    }
}
