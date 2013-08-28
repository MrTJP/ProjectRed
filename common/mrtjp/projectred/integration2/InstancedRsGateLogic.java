package mrtjp.projectred.integration2;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.projectred.ProjectRedIntegration;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;

public abstract class InstancedRsGateLogic extends RedstoneGateLogic<InstancedRsGatePart>
{
    public static InstancedRsGateLogic create(int subID) {
        switch(subID) {
            case 12:
                return new RSLatch();
            case 13:
                return new ToggleLatch();
        }
        return null;
    }
    
    @Override
    public int getOutput(InstancedRsGatePart gate, int r) {
        return (gate.state & 0x10<<r) != 0 ? 15 : 0;
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
    
    public static abstract class ExtraStateLogic extends InstancedRsGateLogic
    {
        public byte state2;
        
        public int state2() {
            return state2&0xFF;
        }
        
        public void setState2(int i) {
            state2 = (byte)i;
        }
        
        @Override
        public void save(NBTTagCompound tag) {
            tag.setByte("state2", state2);
        }
        
        @Override
        public void load(NBTTagCompound tag) {
            state2 = tag.getByte("state2");
        }
        
        @Override
        public void readDesc(MCDataInput packet) {
            if(clientState2())
                state2 = packet.readByte();
        }
        
        @Override
        public void writeDesc(MCDataOutput packet) {
            if(clientState2())
                packet.writeByte(state2);
        }
        
        @Override
        public void read(MCDataInput packet, int switch_key) {
            if(switch_key == 11)
                state2 = packet.readByte();
        }
        
        public boolean clientState2() {
            return false;
        }
        
        public void sendState2Update(GatePart gate) {
            gate.getWriteStream(11).writeByte(state2);
        }
    }
    
    public static class RSLatch extends ExtraStateLogic
    {
        @Override
        public boolean cycleShape(InstancedRsGatePart gate) {
            int newShape = (gate.shape()+1)%4;
            gate.setShape(newShape);
            setState2(GatePart.flipMaskZ(state2()));
            gate.setState(GatePart.flipMaskZ(gate.state()));
            gate.onOutputChange(0xF);
            gate.scheduleTick(2);
            return true;
        }
        
        @Override
        public int outputMask(int shape) {
            return shape >> 1 == 0 ? 0xF : 5;
        }
        
        @Override
        public int inputMask(int shape) {
            return 0xA;
        }
        
        @Override
        public void setup(InstancedRsGatePart gate) {
            setState2(2);
            gate.setState(0x30);
            gate.onOutputChange(0x30);
        }
        
        @Override
        public void onChange(InstancedRsGatePart gate) {
            int stateInput = state2();
            
            int oldInput = gate.state()&0xF;
            int newInput = getInput(gate, 0xA);
            int oldOutput = gate.state()>>4;
            
            if(newInput != oldInput) {
                if(stateInput != 0xA && newInput != 0 && newInput != state2()) {//state needs changing.
                    gate.setState(newInput);
                    setState2(newInput);
                    gate.onOutputChange(oldOutput);//always going low
                    gate.scheduleTick(2);
                }
                else {
                    gate.setState(oldOutput<<4 | newInput);
                    gate.onInputChange();
                }
            }
        }
        
        @Override
        public void scheduledTick(InstancedRsGatePart gate) {
            int oldOutput = gate.state()>>4;
            int newOutput = calcOutput(gate);
            if(oldOutput != newOutput) {
                gate.setState(gate.state() & 0xF | newOutput<<4);
                gate.onOutputChange(outputMask(gate.shape()));
            }
            onChange(gate);
        }
        
        public int calcOutput(SimpleGatePart gate) {
            int input = gate.state()&0xF;
            int stateInput = state2();
            
            if((gate.shape & 1) != 0) {//reverse
                input = GatePart.flipMaskZ(input);
                stateInput = GatePart.flipMaskZ(stateInput);
            }
            
            if(stateInput == 0xA) {//disabled
                if(input == 0xA) {
                    gate.scheduleTick(2);
                    return 0;
                }
                
                if(input == 0)
                    stateInput = gate.world().rand.nextBoolean() ? 2 : 8;
                else
                    stateInput = input;
                setState2(stateInput);
            }
            
            int output = GatePart.shiftMask(stateInput, 1);
            if((gate.shape & 2) == 0)
                output |= stateInput;

            if((gate.shape & 1) != 0)//reverse
                output = GatePart.flipMaskZ(output);
            
            return output;
        }
    }
    
    public static class ToggleLatch extends ExtraStateLogic
    {
        @Override
        public int outputMask(int shape) {
            return 5;
        }
        
        @Override
        public int inputMask(int shape) {
            return 0xA;
        }
        
        @Override
        public boolean clientState2() {
            return true;
        }
        
        @Override
        public void setup(InstancedRsGatePart gate) {
            gate.setState(0x10);
            gate.onOutputChange(1);
        }
        
        @Override
        public void onChange(InstancedRsGatePart gate) {
            int oldInput = gate.state()&0xF;
            int newInput = getInput(gate, 0xA);
            int high = newInput & ~oldInput;
            
            if(high == 2 || high == 8)//one side went high (if both, double change so no change)
                toggle(gate);
            
            if(oldInput != newInput) {
                gate.setState(gate.state() & 0xF0 | newInput);
                gate.onInputChange();
            }
        }

        @Override
        public void scheduledTick(InstancedRsGatePart gate) {
            int oldOutput = gate.state()>>4;
            int newOutput = state2 == 0 ? 1 : 4;
            if(oldOutput != newOutput) {
                gate.setState(newOutput<<4 | gate.state() & 0xF);
                gate.onOutputChange(outputMask(5));
            }
            onChange(gate);
        }
        
        @Override
        public boolean activate(InstancedRsGatePart gate, EntityPlayer player, ItemStack held) {
            if(held == null || held.getItem() != ProjectRedIntegration.itemScrewdriver) {
                if(!gate.world().isRemote)
                    toggle(gate);
                return true;
            }
            return false;
        }

        private void toggle(InstancedRsGatePart gate) {
            setState2(state2^1);
            sendState2Update(gate);
            gate.scheduleTick(2);
        }
    }
}
