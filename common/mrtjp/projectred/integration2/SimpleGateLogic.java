
package mrtjp.projectred.integration2;

import java.util.Random;

import mrtjp.projectred.ProjectRedIntegration;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;

public abstract class SimpleGateLogic extends RedstoneGateLogic<SimpleGatePart>
{
    public static int[] advanceDead = new int[]{
        1, 2, 4, 0, 5, 6, 3};
    
    public static SimpleGateLogic[] instances = getInstances();
    
    public static SimpleGateLogic[] getInstances() {
        return new SimpleGateLogic[]{
                new OR(),
                new NOR(),
                new NOT(),
                new AND(),
                null,
                null,
                null,
                null,
                null,
                new Pulse(),
                new Repeater(),
                new Randomizer()
            };
    }

    public static int countBits(int n) {
        int c;
        for (c = 0; n != 0; c++) 
          n &= n - 1; // clear the least significant bit set
        return c;
    }
    
    public boolean getOutput(SimpleGatePart gate, int r) {
        return (gate.state & 0x10<<r) != 0;
    }
    
    public int cycleShape(int shape) {
        do 
            shape = advanceDead[shape];
        while(countBits(shape) > maxDeadSides() && 
                32-Integer.numberOfLeadingZeros(shape) <= maxDeadSides());
        return shape;
    }
    
    public int deadSides() {
        return 0;
    }
    
    public int maxDeadSides() {
        return deadSides()-1;
    }
    
    public int getDelay(int shape) {
        return 2;
    }
    
    public int feedbackMask(int shape) {
        return 0;
    }
    
    @Override
    public void onChange(SimpleGatePart gate) {
        int inputMask = inputMask(gate.shape());
        int outputMask = outputMask(gate.shape());
        int feedbackMask = feedbackMask(gate.shape());
        int oldInput = gate.state&0xF;
        int newInput = getInput(gate, inputMask|feedbackMask);
        if(oldInput != newInput) {
            gate.setState(gate.state & 0xF0 | newInput);
            gate.onInputChange();
        }
        
        int newOutput = getOutput(gate.state&inputMask) & outputMask;
        if(newOutput != gate.state>>4)
            gate.scheduleTick(getDelay(gate.shape()));//we need to update our output state some ticks from now
    }
    
    @Override
    public void scheduledTick(SimpleGatePart gate) {
        int inputMask = inputMask(gate.shape());
        int outputMask = outputMask(gate.shape());
        int oldOutput = gate.state>>4;
        int newOutput = getOutput(gate.state&inputMask) & outputMask;
        if(oldOutput != newOutput) {
            gate.setState(gate.state & 0xF | newOutput<<4);
            gate.onOutputChange(outputMask);
        }
        onChange(gate);
    }
    
    @Override
    public void setup(SimpleGatePart gate) {
        instances = getInstances();
        int inputMask = inputMask(gate.shape());
        int outputMask = outputMask(gate.shape());
        int output = getOutput(getInput(gate, inputMask)) & outputMask;
        if(output != 0) {
            gate.setState(output << 4);
            gate.onOutputChange(output);//can use output for output mask because nothing is going low
        }
    }

    public static class OR extends SimpleGateLogic
    {
        @Override
        public int inputMask(int shape) {
            return ~shape<<1 & 0xE;
        }
        
        @Override
        public int deadSides() {
            return 3;
        }
        
        @Override
        public int getOutput(int input) {
            return input != 0 ? 1 : 0;
        }
    }
    
    public static class NOR extends SimpleGateLogic
    {
        @Override
        public int feedbackMask(int shape) {
            return 1;
        }

        @Override
        public int inputMask(int shape) {
            return ~shape<<1 & 0xE;
        }
        
        @Override
        public int deadSides() {
            return 3;
        }
        
        @Override
        public int getOutput(int input) {
            return input == 0 ? 1 : 0;
        }
    }
    
    public static class NOT extends SimpleGateLogic
    {
        @Override
        public int feedbackMask(int shape) {
            return outputMask(shape);
        }
        
        @Override
        public int outputMask(int shape) {
            int m = 0;
            m |= (shape & 1)<<1;//first dead side is right
            m |= (shape & 2)>>1;//second dead side is back
            m |= (shape & 4)<<1;//third dead side is left
            return ~m & 0xB;
        }
        
        @Override
        public int inputMask(int shape) {
            return 4;
        }
        
        @Override
        public int deadSides() {
            return 3;
        }
        
        @Override
        public int getOutput(int input) {
            return input == 0 ? 0xB : 0;
        }
    }
    
    public static class AND extends SimpleGateLogic
    {
        @Override
        public int inputMask(int shape) {
            return ~shape<<1 & 0xE;
        }
        
        @Override
        public int deadSides() {
            return 3;
        }

        @Override
        public int getOutput(int input) {
            return 0;
        }
        
    }
    
    public static class Pulse extends SimpleGateLogic
    {
        @Override
        public int getOutput(int input) {//after 2 ticks, output is always returned to 0
            return 0;
        }
        
        @Override
        public int inputMask(int shape) {
            return 4;
        }
        
        @Override
        public void onChange(SimpleGatePart gate) {
            int oldInput = gate.state&0xF;
            int newInput = getInput(gate, 4);
            if(oldInput != newInput) {
                gate.setState(gate.state & 0xF0 | newInput);
                gate.onInputChange();
                
                if(newInput != 0 && (gate.state & 0xF0) == 0) {//input was set high, output is low, set output high, wait 2 ticks
                    gate.setState(gate.state & 0xF | 0x10);
                    gate.scheduleTick(2);
                    gate.onOutputChange(1);
                }
            }
        }
    }
    
    public static class Repeater extends SimpleGateLogic
    {
        public int[] delays = new int[]{2, 4, 6, 8, 16, 32, 64, 128, 256};
        
        @Override
        public int getOutput(int input) {
            return input == 0 ? 0 : 1;
        }
        
        @Override
        public int inputMask(int shape) {
            return 4;
        }
        
        @Override
        public void onChange(SimpleGatePart gate) {
            if(gate.schedTime < 0)//only accept input changes when not scheduled
                super.onChange(gate);
        }
        
        @Override
        public int getDelay(int shape) {
            return delays[shape];
        }
        
        @Override
        public int cycleShape(int shape) {
            return (shape+1)%delays.length;
        }
        
        @Override
        public boolean activate(SimpleGatePart part, EntityPlayer player, ItemStack held) {
            if(held == null || held.getItem() != ProjectRedIntegration.itemScrewdriver) {
                if(!part.world().isRemote)
                    part.configure();
                return true;
            }
            return false;
        }
    }
    
    public static class Randomizer extends SimpleGateLogic
    {
        public Random rand = new Random();
        
        @Override
        public int getOutput(int input) {
            return input == 0 ? 0 : GatePart.shiftMask(rand.nextInt(8), 3);
        }
        
        @Override
        public int outputMask(int shape) {
            return 0xB;
        }
        
        @Override
        public int feedbackMask(int shape) {
            return 0xB;
        }
        
        @Override
        public int inputMask(int shape) {
            return 4;
        }
        
        @Override
        public void onChange(SimpleGatePart gate) {
            int oldInput = gate.state&0xF;
            int newInput = getInput(gate, 0xF);
            
            if(oldInput != newInput) {
                gate.setState(gate.state & 0xF0 | newInput);
                gate.onInputChange();
            }
            
            if(newInput != 0)//only schedule a new tick when we are high, otherwise, leave our output state
                gate.scheduleTick(4 + rand.nextInt(6));
        }
        
        @Override
        public void scheduledTick(SimpleGatePart gate) {
            if((gate.state & 4) != 0)
                super.scheduledTick(gate);
        }
    }
}
