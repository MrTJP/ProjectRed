package mrtjp.projectred.integration;

import mrtjp.projectred.api.IBundledEmitter;
import mrtjp.projectred.api.IConnectable;
import net.minecraft.nbt.NBTTagCompound;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;

import static mrtjp.projectred.transmission.BundledCableCommons.*;

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
        return null;
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
        public byte[] input0, output0, input2, output2;
        
        public BusTransceiver(BundledGatePart gate) {
            super(gate);
        }
        
        public void save(NBTTagCompound tag) {
            saveSignal(tag, "in0", input0);
            saveSignal(tag, "out0", output0);
            saveSignal(tag, "in2", input2);
            saveSignal(tag, "out2", output2);
        }
        
        public void load(NBTTagCompound tag) {
            input0 = loadSignal(tag, "in0");
            input2 = loadSignal(tag, "in2");
            output0 = loadSignal(tag, "out0");
            output2 = loadSignal(tag, "out2");
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
            return packDigital(output0)|packDigital(output2)<<16;
        }
        
        public void unpackClientData(int packed) {
            output0 = unpackDigital(output0, packed&0xFFFF);
            output2 = unpackDigital(output2, packed>>>16);
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
            return r == 0 ? output0 : output2;
        }
        
        public byte[] getBundledInput(int input, int r) {
            return raiseSignal(copySignal(gate.getBundledInput(r)), getBundledOutput(gate, r));//or'd with our output
        }
        
        @Override
        public void onChange(BundledGatePart gate) {
            int oldInput = gate.state() & 0xF;
            int newInput = getInput(gate, 2|8);
            if(oldInput != newInput) {
                gate.setState(gate.state() & 0xF0|newInput);
                gate.onInputChange();
            }

            byte[] newInput0 = getBundledInput(newInput, 0);
            if (!signalsEqual(input0, newInput0)) {
                input0 = newInput0;
                gate.onInputChange();
            }
            
            byte[] newInput2 = getBundledInput(newInput, 2);
            if (!signalsEqual(input2, newInput2)) {
                input2 = newInput2;
                gate.onInputChange();
            }

            if (gate.shape() == 1)//flip for output calc
                newInput = GatePart.flipMaskZ(newInput);
            
            if(!signalsEqual(output0, calcBundledOutput(0)) ||
                    !signalsEqual(output2, calcBundledOutput(2)))
                gate.scheduleTick(2);
        }
        
        public byte[] calcBundledOutput(int r) {
            int input = gate.state() & 0xF;
            if (gate.shape() == 1)
                input = GatePart.flipMaskZ(input);
            
            if(r == 0)
                return (input&2) != 0 ? input2 : null;
            if(r == 2)
                return (input&8) != 0 ? input0 : null;
                
            return null;
        }
        
        @Override
        public void scheduledTick(BundledGatePart gate) {
            output0 = calcBundledOutput(0);
            output2 = calcBundledOutput(2);
            
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
