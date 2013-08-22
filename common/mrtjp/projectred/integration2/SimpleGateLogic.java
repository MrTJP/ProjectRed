
package mrtjp.projectred.integration2;

public abstract class SimpleGateLogic extends RedstoneGateLogic<SimpleGatePart>
{
    public static int[] advanceDead = new int[]{
        1, 2, 4, 0, 5, 6, 3};
    
    public static SimpleGateLogic[] instances = new SimpleGateLogic[]{
        new OR(),
        new NOR(),
        new NOT(),
    };

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
        while(countBits(shape) > deadSides());
        return shape;
    }
    
    public int deadSides() {
        return 0;
    }
    
    public int getDelay(int shape) {
        return 2;
    }
    
    public int feedbackMask() {
        return 0;
    }
    
    @Override
    public void onChange(SimpleGatePart gate) {
        int inputMask = inputMask(gate.shape());
        int feedbackMask = feedbackMask();
        int oldInput = gate.state&0xF;
        int newInput = getInput(gate, inputMask|feedbackMask);
        if(oldInput != newInput) {
            gate.setState(gate.state & 0xF0 | newInput);
            gate.scheduleTick(getDelay(gate.shape()));
            gate.onInputChange();
        }
    }
    
    @Override
    public void scheduledTick(SimpleGatePart gate) {
        int inputMask = inputMask(gate.shape());
        int outputMask = outputMask(gate.shape());
        int oldOutput = gate.state>>4;
        int newOutput = getOutput(getInput(gate, inputMask));
        if(oldOutput != newOutput) {
            gate.setState(gate.state & 0xF | newOutput<<4);
            gate.onOutputChange(outputMask);
        }
        onChange(gate);
    }
    
    @Override
    public void setup(SimpleGatePart gate) {
        int inputMask = inputMask(gate.shape());
        int output = getOutput(getInput(gate, inputMask));
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
            return 2;
        }
        
        @Override
        public int getOutput(int input) {
            return input != 0 ? 1 : 0;
        }
    }
    
    public static class NOR extends SimpleGateLogic
    {
        // unfinished
        @Override
        public int getOutput(int input) {
            return 0;
        }
    }
    
    public static class NOT extends SimpleGateLogic
    {
        public int feedbackMask() {
            return 0xB;
        }
        
        @Override
        public int inputMask(int shape) {
            return 4;
        }
        
        @Override
        public int getOutput(int input) {
            return input == 0 ? 0xB : 0;
        }
        
    }
}
