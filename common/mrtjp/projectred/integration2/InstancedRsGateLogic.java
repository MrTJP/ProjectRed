package mrtjp.projectred.integration2;

import mrtjp.projectred.ProjectRedIntegration;
import mrtjp.projectred.core.Configurator;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;

public abstract class InstancedRsGateLogic extends RedstoneGateLogic<InstancedRsGatePart>
{
    public static InstancedRsGateLogic create(InstancedRsGatePart gate, int subID) {
        switch(subID) {
            case 12:
                return new RSLatch(gate);
            case 13:
                return new ToggleLatch(gate);
            case 17:
                return new Timer(gate);
            case 18:
                return new Sequencer(gate);
            case 19:
                return new Counter(gate);
            case 20:
                return new StateCell(gate);
            case 21:
                return new Synchronizer(gate);
        }
        throw new IllegalArgumentException("Invalid subID: "+subID);
    }
    
    public InstancedRsGateLogic(InstancedRsGatePart gate) {
        this.gate = gate;
    }
    
    public InstancedRsGatePart gate;
    
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
        
        public ExtraStateLogic(InstancedRsGatePart gate) {
            super(gate);
        }
        
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
        
        public void sendState2Update() {
            gate.getWriteStream(11).writeByte(state2);
        }
    }
    
    public static class RSLatch extends ExtraStateLogic
    {
        public RSLatch(InstancedRsGatePart gate) {
            super(gate);
        }
        

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
        public ToggleLatch(InstancedRsGatePart gate) {
            super(gate);
        }

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
            sendState2Update();
            gate.scheduleTick(2);
            if (Configurator.logicGateSounds.getBoolean(true))
                gate.world().playSoundEffect(gate.x()+0.5D, gate.y()+0.5D, gate.z()+0.5D, "random.click", 0.3F, 0.5F);
        }
    }
    
    public static abstract class TimerGateLogic extends InstancedRsGateLogic implements ITimerGuiLogic
    {
        public int pointer_max = 38;
        public long pointer_start = -1;

        public TimerGateLogic(InstancedRsGatePart gate) {
            super(gate);
        }
        
        @Override
        public void save(NBTTagCompound tag) {
            tag.setInteger("pmax", pointer_max);
            tag.setLong("pstart", pointer_start);
        }
        
        @Override
        public void load(NBTTagCompound tag) {
            pointer_max = tag.getInteger("pmax");
            pointer_start = tag.getLong("pstart");
        }
        
        @Override
        public void writeDesc(MCDataOutput packet) {
            packet.writeInt(pointer_max);
            packet.writeLong(pointer_start);
        }
        
        @Override
        public void readDesc(MCDataInput packet) {
            pointer_max = packet.readInt();
            pointer_start = packet.readLong();
        }
        
        @Override
        public void read(MCDataInput packet, int switch_key) {
            if(switch_key == 12)
                pointer_max = packet.readInt();
            else if(switch_key == 13) {
                pointer_start = packet.readInt();
                if(pointer_start >= 0)
                    pointer_start = gate.world().getWorldTime()-pointer_start;
            }
        }
        
        public int pointerValue() {
            if(pointer_start < 0)
                return 0;
            
            return (int)(gate.world().getWorldTime()-pointer_start);
        }
        
        public void sendPointerMaxUpdate() {
            gate.getWriteStream(12).writeInt(pointer_max);
        }
        
        public void sendPointerUpdate() {
            gate.getWriteStream(13).writeInt(pointer_start < 0 ? -1 : pointerValue());
        }
        
        @Override
        public int getTimerMax() {
            return pointer_max+2;
        }
        
        @Override
        public void setTimerMax(GatePart gate, int t) {
            if(t < 4)
                t = 4;
            if(t != pointer_max) {
                pointer_max = t-2;
                sendPointerMaxUpdate();
            }
        }
        
        @Override
        public void onTick(InstancedRsGatePart gate) {
            if(pointer_start >= 0) {
                if(gate.world().getWorldTime() >= pointer_start+pointer_max)
                    pointerTick();
                else if(pointer_start > gate.world().getWorldTime())
                    pointer_start = gate.world().getWorldTime();
            }
        }
        
        public abstract void pointerTick();
        
        public void resetPointer() {
            if(pointer_start >= 0) {
                pointer_start = -1;
                if(!gate.world().isRemote)
                    sendPointerUpdate();
            }
        }
        
        public void startPointer() {
            if(pointer_start < 0) {
                pointer_start = gate.world().getWorldTime();
                if(!gate.world().isRemote)
                    sendPointerUpdate();
            }
        }
        
        public float interpPointer(float f) {
            if(pointer_start < 0)
                return 0;
            
            return (pointerValue()+f)/pointer_max;
        }
        
        @Override
        public boolean activate(InstancedRsGatePart gate, EntityPlayer player, ItemStack held) {
            if(held == null || held.getItem() != ProjectRedIntegration.itemScrewdriver) {
                if(!gate.world().isRemote)
                    IntegrationSPH.openTimerGui(player, gate);
                
                return true;
            }
            return false;
        }
    }
    
    public static class Timer extends TimerGateLogic
    {
        public Timer(InstancedRsGatePart gate) {
            super(gate);
        }
        
        @Override
        public int outputMask(int shape) {
            return 0xB;
        }
        
        @Override
        public int inputMask(int shape) {
            return 0xE;
        }
        
        @Override
        public void setup(InstancedRsGatePart gate) {
            startPointer();
        }
        
        @Override
        public void scheduledTick(InstancedRsGatePart gate) {
            gate.setState(gate.state()&0xF);
            gate.onOutputChange(0xB);
            onChange(gate);
        }
        
        @Override
        public void onChange(InstancedRsGatePart gate) {
            int oldInput = gate.state()&0xF;
            int newInput = getInput(gate, 0xE);
            if(newInput != oldInput) {
                gate.setState(gate.state()&0xF0 | newInput);
                gate.onInputChange();
            }
            
            if(gate.schedTime < 0) {
                if(newInput > 0)
                    resetPointer();
                else
                    startPointer();
            }
        }
        
        @Override
        public void pointerTick() {
            resetPointer();
            if (Configurator.logicGateSounds.getBoolean(true))
                gate.world().playSoundEffect(gate.x()+0.5D, gate.y()+0.5D, gate.z()+0.5D, "random.click", 0.3F, 0.5F);
            if(!gate.world().isRemote) {
                gate.setState(0xB0 | gate.state()&0xF);
                gate.onOutputChange(0xB);
                gate.scheduleTick(2);
            }
        }
    }
    
    public static class StateCell extends TimerGateLogic
    {
        public byte state2;
        
        public StateCell(InstancedRsGatePart gate) {
            super(gate);
        }
        
        public int state2() {
            return state2&0xFF;
        }
        
        public void setState2(int i) {
            state2 = (byte)i;
        }
        
        @Override
        public void save(NBTTagCompound tag) {
            super.save(tag);
            tag.setByte("state2", state2);
        }
        
        @Override
        public void load(NBTTagCompound tag) {
            super.load(tag);
            state2 = tag.getByte("state2");
        }
        
        @Override
        public void readDesc(MCDataInput packet) {
            super.readDesc(packet);
            state2 = packet.readByte();
        }
        
        @Override
        public void writeDesc(MCDataOutput packet) {
            super.writeDesc(packet);
            packet.writeByte(state2);
        }
        
        @Override
        public void read(MCDataInput packet, int switch_key) {
            if(switch_key == 11)
                state2 = packet.readByte();
            else
                super.read(packet, switch_key);
        }
        
        public void sendState2Update() {
            gate.getWriteStream(11).writeByte(state2);
        }
        
        @Override
        public boolean cycleShape(InstancedRsGatePart gate) {
            gate.setShape((gate.shape()+1)%2);
            return true;
        }
        
        @Override
        public int outputMask(int shape) {
            int output = 9;
            if(shape == 1)
                output = GatePart.flipMaskZ(output);
            
            return output;
        }
        
        @Override
        public int inputMask(int shape) {
            int input = 6;
            if(shape == 1)
                input = GatePart.flipMaskZ(input);
            
            return input;
        }
        
        @Override
        public void onChange(InstancedRsGatePart gate) {
            int oldInput = gate.state()&0xF;
            int newInput = getInput(gate, 0xE);
            
            if(oldInput != newInput) {
                gate.setState(gate.state()&0xF0 | newInput);
                gate.onInputChange();
                
                if(gate.shape() == 1)
                    newInput = GatePart.flipMaskZ(newInput);
                
                if((newInput & 4) != 0 && state2 == 0) {
                    setState2(1);
                    sendState2Update();
                    gate.scheduleTick(2);
                }
                
                if(state2 != 0) {
                    if((newInput & 6) != 0)
                        resetPointer();
                    else
                        startPointer();
                }
            }
        }

        @Override
        public void pointerTick() {
            resetPointer();
            if(!gate.world().isRemote) {
                setState2(0);
                sendState2Update();
                gate.setState(0x10 | gate.state()&0xF);
                gate.onOutputChange(outputMask(gate.shape()));
                gate.scheduleTick(2);
            }
        }

        @Override
        public void scheduledTick(InstancedRsGatePart gate) {
            int output = 0;
            if(state2 != 0)
                output = 8;
            if(gate.shape() == 1)
                output = GatePart.flipMaskZ(output);
            gate.setState(output<<4 | gate.state()&0xF);
            gate.onOutputChange(outputMask(gate.shape()));
        }
    }
    
    public static class Sequencer extends InstancedRsGateLogic implements ITimerGuiLogic
    {

        public int pointer_max = 40;
        
        public Sequencer(InstancedRsGatePart gate) {
            super(gate);
        }

        @Override
        public void onChange(InstancedRsGatePart gate) {
        }

        @Override
        public void scheduledTick(InstancedRsGatePart gate) {   
        }

        @Override
        public int getTimerMax() {
            return pointer_max;
        }
        
        @Override
        public void setTimerMax(GatePart gate, int t) {
            if(t < 4)
                t = 4;
            if(t != pointer_max) {
                pointer_max = t;
                sendPointerMaxUpdate();
            }
        }
        
        @Override
        public void save(NBTTagCompound tag) {
            tag.setInteger("pmax", pointer_max);
        }
        
        @Override
        public void load(NBTTagCompound tag) {
            pointer_max = tag.getInteger("pmax");
        }
        
        @Override
        public void writeDesc(MCDataOutput packet) {
            packet.writeInt(pointer_max);
        }
        
        @Override
        public void readDesc(MCDataInput packet) {
            pointer_max = packet.readInt();
        }
        
        @Override
        public void read(MCDataInput packet, int switch_key) {
            if(switch_key == 11)
                pointer_max = packet.readInt();
        }

        public void sendPointerMaxUpdate() {
            gate.getWriteStream(11).writeInt(pointer_max);
        }
        
        @Override
        public void onTick(InstancedRsGatePart gate) {
            if (!gate.world().isRemote) {
                int oldOut = gate.state()>>4;
                int out = 1<<gate.world().getWorldTime()%pointer_max/(pointer_max/4);
                if (gate.shape() == 1)
                    out = GatePart.flipMaskZ(out);
                if (oldOut != out) {
                    gate.setState(out<<4);
                    gate.onOutputChange(0xF);
                    if (Configurator.logicGateSounds.getBoolean(true))
                        gate.world().playSoundEffect(gate.x()+0.5D, gate.y()+0.5D, gate.z()+0.5D, "random.click", 0.3F, 0.5F);
                }
            }
        }
        
        @Override
        public boolean cycleShape(InstancedRsGatePart gate) {
            gate.setShape(gate.shape() == 1 ? 0 : 1);
            return true;
        }
        
        @Override
        public boolean activate(InstancedRsGatePart gate, EntityPlayer player, ItemStack held) {
            if(held == null || held.getItem() != ProjectRedIntegration.itemScrewdriver) {
                if(!gate.world().isRemote)
                    IntegrationSPH.openTimerGui(player, gate);
                return true;
            }
            return false;
        }
        
        @Override
        public int outputMask(int shape) {
            return 0xF;
        }
        
        @Override
        public int inputMask(int shape) {
            return 0;
        }
    }
    
    public static class Counter extends InstancedRsGateLogic implements ICounterGuiLogic
    {
        public int value = 0;
        public int max = 10;
        public int incr = 1;
        public int decr = 1;
        
        public Counter(InstancedRsGatePart gate) {
            super(gate);
        }
        
        @Override
        public void save(NBTTagCompound tag) {
            tag.setInteger("val", value);
            tag.setInteger("max", max);
            tag.setInteger("inc", incr);
            tag.setInteger("dec", decr);
        }
        
        @Override
        public void load(NBTTagCompound tag) {
            value = tag.getInteger("val");
            max = tag.getInteger("max");
            incr = tag.getInteger("inc");
            decr = tag.getInteger("dec");
        }

        @Override
        public int getCounterValue() {
            return value;
        }
        
        @Override
        public int getCounterMax() {
            return max;
        }

        @Override
        public int getCounterIncr() {
            return incr;
        }

        @Override
        public int getCounterDecr() {
            return decr;
        }
        
        @Override
        public void setCounterValue(GatePart gate, int i) {
            int oldVal = value;
            value = Math.min(max, Math.max(0, i));
            if (value != oldVal)
                sendValueUpdate();
        }

        @Override
        public void setCounterMax(GatePart gate, int i) {
            int oldMax = max;
            max = Math.min(32767, Math.max(1, i));
            if (max != oldMax)
                sendMaxUpdate();
        }

        @Override
        public void setCounterIncr(GatePart gate, int i) {
            int oldIncr = incr;
            incr = Math.min(max, Math.max(1, i));
            if (incr != oldIncr)
                sendIncrUpdate();
        }

        @Override
        public void setCounterDecr(GatePart gate, int i) {
            int oldDecr = decr;
            decr = Math.min(max, Math.max(1, i));
            if (decr != oldDecr)
                sendDecrUpdate();
        }

        @Override
        public void read(MCDataInput packet, int switch_key) {
            if(switch_key == 11)
                value = packet.readInt();
            else if(switch_key == 12)
                max = packet.readInt();
            else if(switch_key == 13)
                incr = packet.readInt();
            else if(switch_key == 14)
                decr = packet.readInt();
        }

        public void sendValueUpdate() {
            gate.getWriteStream(11).writeInt(value);
        }
        public void sendMaxUpdate() {
            gate.getWriteStream(12).writeInt(max);
        }
        public void sendIncrUpdate() {
            gate.getWriteStream(13).writeInt(incr);
        }
        public void sendDecrUpdate() {
            gate.getWriteStream(14).writeInt(decr);
        }

        @Override
        public void onChange(InstancedRsGatePart gate) {
            int oldInput = gate.state()&0xF;
            int newInput = getInput(gate, 0xA);
            if (gate.shape() == 1)
                newInput = GatePart.flipMaskZ(newInput);
            int high = newInput & ~oldInput;
            if ((high & 2) != 0)
                setCounterValue(gate, value + incr);

            if ((high & 8) != 0)
                setCounterValue(gate, value - decr);
            
            if(oldInput != newInput) {
                gate.setState(gate.state() & 0xF0 | newInput);
                gate.onInputChange();
                gate.scheduleTick(2);
            }   
        }

        @Override
        public boolean cycleShape(InstancedRsGatePart gate) {
            gate.setShape(gate.shape() == 1 ? 0 : 1);
            return true;
        }

        @Override
        public void scheduledTick(InstancedRsGatePart gate) {
            int oldOutput = gate.state();
            int newOutput = 0;
            if (value == max)
                newOutput = 1;
            else if (value == 0)
                newOutput = 4;
            
            if (newOutput != oldOutput)
                gate.setState(gate.state()&0xF | newOutput<<4);
            
            if (newOutput != oldOutput)
                gate.onOutputChange(5);
        }
        
        @Override
        public int outputMask(int shape) {
            return 1 | 4;
        }
        
        @Override
        public int inputMask(int shape) {
            return 2 | 8;
        }
        
        @Override
        public boolean activate(InstancedRsGatePart gate, EntityPlayer player, ItemStack held) {
            if(held == null || held.getItem() != ProjectRedIntegration.itemScrewdriver) {
                if(!gate.world().isRemote)
                    IntegrationSPH.openCounterGui(player, gate);
                return true;
            }
            return false;
        }
    }
    
    public static class Synchronizer extends ExtraStateLogic 
    {

        public Synchronizer(InstancedRsGatePart gate) {
            super(gate);
        }

        @Override
        public void onChange(InstancedRsGatePart gate) {
            int oldInput = gate.state() & 0xF;
            int newInput = getInput(gate, 2 | 4 | 8);
            int high = newInput & ~oldInput;
            if (oldInput != newInput) {
                gate.setState(gate.state() & 0xF0 | newInput);
                gate.onInputChange();
                
                if ((newInput & 4) != 0) {
                    off();
                } else {
                    if ((high & 2) != 0)
                        setRight();
                    if ((high & 8) != 0)
                        setLeft();
                }

                if (right() && left())
                    gate.scheduleTick(2);
            }
        }

        @Override
        public void scheduledTick(InstancedRsGatePart gate) {
            if (!pulsing() && right() && left()) {
                gate.setState(gate.state() | 1 << 4);
                gate.onOutputChange(1);
                setPulsing();
                gate.scheduleTick(2);
            } else if (pulsing()){
                gate.setState(gate.state() & ~(0x10));
                gate.onOutputChange(1);
                off();
            }
        }

        public boolean right() {
            return (state2() & 1) != 0;
        }
        
        public boolean left() {
            return (state2() & 1<<1) != 0;
        }
        
        public boolean pulsing() {
            return (state2() & 1<<2) != 0;
        }
        
        public void setPulsing() {
            int oldValue = state2();
            setState2(state2() | 1<<2);
            if (state2() != oldValue)
                sendState2Update();
        }
        
        public void setRight() {
            int oldValue = state2();
            setState2(state2() | 1);
            if (state2() != oldValue)
                sendState2Update();
        }
        
        public void setLeft() {
            int oldValue = state2();
            setState2(state2() | 1<<1);
            if (state2() != oldValue)
                sendState2Update();
        }
        
        public void off() {
            int oldValue = state2();
            setState2(0);
            if (state2() != oldValue)
                sendState2Update();
        }
        
        @Override
        public int outputMask(int shape) {
            return 1;
        }
        
        @Override
        public int inputMask(int shape) {
            return 2 | 4 | 8;
        }
    }

}
