package mrtjp.projectred.integration;

import mrtjp.projectred.api.IBundledEmitter;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.transmission.BundledCableCommons;
import net.minecraft.nbt.NBTTagCompound;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;

public abstract class BundledGateLogic extends RedstoneGateLogic<BundledGatePart> {

    public static BundledGateLogic create(BundledGatePart gate, int subID) {
        if (subID == 22)
            return new BusTransceiver(gate);
        return null;
    }

    public BundledGateLogic(BundledGatePart gate) {
        this.gate = gate;
    }
    
    public BundledGatePart gate;

    @Override
    public boolean canConnectTo(BundledGatePart gate, IConnectable wire, int r) {
        if (super.canConnectTo(gate, wire, r))
            return true;
        if (wire instanceof IBundledEmitter)
            return canConnectBundled(gate, r);
        return false;
    }

    public boolean canConnectBundled(BundledGatePart gate, int r) {
        return canConnectBundled(gate.shape(), r);
    }

    public boolean canConnectBundled(int shape, int r) {
        return ((bundledInputMask(shape) | bundledOutputMask(shape)) & 1 << r) != 0;
    }

    public int bundledInputMask(int shape) {
        return 0;
    }

    public int bundledOutputMask(int shape) {
        return 0;
    }

    @Override
    public int getOutput(BundledGatePart gate, int r) {
        return (gate.state & 0x10<<r) != 0 ? 15 : 0;
    }

    public byte[] getBundledOutput(BundledGatePart gate, int r) {
        return new byte[16];
    }
    
    @Override
    public void onChange(BundledGatePart gate) {
    }

    @Override
    public void scheduledTick(BundledGatePart gate) {
    }
    
    public void save(NBTTagCompound tag) {
    }
    
    public void load(NBTTagCompound tag) {
    }

    public void readDesc(MCDataInput packet) {
    }
    
    public void writeDesc(MCDataOutput packet) {
    }
    
    public void read(MCDataInput packet, int switch_key) {
    }

    public static class BusTransceiver extends BundledGateLogic
    {
        public byte[] input0 = new byte[16];
        public byte[] input2 = new byte[16];
                
        public BusTransceiver(BundledGatePart gate) {
            super(gate);
        }
        
        public void save(NBTTagCompound tag) {
            tag.setByteArray("in0", input0);
            tag.setByteArray("in2", input2);
        }
        
        public void load(NBTTagCompound tag) {
            input0 = tag.getByteArray("in0");
            input2 = tag.getByteArray("in2");
        }
        
        @Override
        public void readDesc(MCDataInput packet) {
            unpackClientData(packet.readInt());
        }
        
        @Override
        public void writeDesc(MCDataOutput packet) {
            packet.writeInt(packClientData());
        }

        public void read(MCDataInput packet, int switch_key) {
            if (switch_key == 11)
                unpackClientData(packet.readInt());
        }
        
        public void sendClientUpdate() {
            gate.getWriteStream(11).writeInt(packClientData());
        }
        
        public int packClientData() {
            int packed = 0;
            for (int i = 0; i < 16; i++) {
                if (input0[i] != 0)
                    packed |= 1<<i;
                if (input2[i] != 0)
                    packed |= 1<<(i+16);
            }
            return packed;
        }
        
        public void unpackClientData(int packed) {
            input0 = new byte[16];
            input2 = new byte[16];
            for (int i = 0; i < 16; i++) {
                if ((packed & 1<<i) != 0)
                    input0[i] = (byte) 255;
                if ((packed & 1<<(i+16)) != 0)
                    input2[i] = (byte) 255;
            }
        }
        
        public int bundledInputMask(int shape) {
            return 1|4;
        }

        public int bundledOutputMask(int shape) {
            return 1|4;
        }
        
        public int inputMask(int shape) {
            return 2|8;
        }
        
        public int outputMask(int shape) {
            return 0;
        }

        public byte[] getBundledOutput(BundledGatePart gate, int r) {
            return r == gate.rotation() ? input0 : input2;
        }
        
        public byte[] getBundledInput(int input, int r) {
            byte[] signal = gate.getBundledInput(r);
            signal = signal == null ? new byte[16] : signal.clone();
            
            if((input & 1<<(r+1)%4) != 0)
                signal = BundledCableCommons.raiseSignal(signal, getBundledOutput(gate, r));
            
            return signal;
        }
        
        @Override
        public void onChange(BundledGatePart gate) {
            int oldInput = gate.state() & 0xF;
            int newInput = getInput(gate, 2|8);
            if (gate.shape() == 1)
                newInput = GatePart.flipMaskZ(newInput);
            
            if(oldInput != newInput) {
                gate.setState(gate.state() & 0xF0|newInput);
                gate.onInputChange();
                gate.scheduleTick(2);
            }

            byte[] newInput0 = (newInput&8) != 0 ? getBundledInput(newInput, 0) : new byte[16];
            if (!BundledCableCommons.signalsEqual(input0, newInput0)) {
                input0 = newInput0;
                gate.scheduleTick(2);
            }
            
            byte[] newInput2 = (newInput&2) != 0 ? getBundledInput(newInput, 2) : new byte[16];
            if (!BundledCableCommons.signalsEqual(input2, newInput2)) {
                input2 = newInput2;
                gate.scheduleTick(2);
            }
        }
        
        @Override
        public void scheduledTick(BundledGatePart gate) {
            gate.onOutputChange(1|4);
            sendClientUpdate();
        }

        @Override
        public boolean cycleShape(BundledGatePart gate) {
            gate.setShape(gate.shape() == 0 ? 1 : 0);
            return true;
        }
    }
}
