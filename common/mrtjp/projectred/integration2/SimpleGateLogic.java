
package mrtjp.projectred.integration2;

public abstract class SimpleGateLogic extends RedstoneGateLogic<SimpleGatePart>
{
    public static int[] advanceDead = new int[]{
        1, 2, 4, 0, 5, 6, 3};
    
    public static int countBits(int n) {
        int c;
        for (c = 0; n != 0; c++) 
          n &= n - 1; // clear the least significant bit set
        return c;
    }
    
    public static SimpleGateLogic[] instances = new SimpleGateLogic[]{
            new OR()
        };
    
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
    
    public boolean getOutput(SimpleGatePart gate, int r) {
        return (gate.state & outputMask(gate.shape()) & 1<<r) != 0;
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
    
    @Override
    public void onChange(SimpleGatePart gate) {
        int inputMask = inputMask(gate.shape());
        int oldInput = gate.state&inputMask;
        int newInput = getInput(gate, inputMask);
        if(oldInput != newInput) {
            gate.setState(gate.state & outputMask(gate.shape()) | newInput);
            gate.scheduleTick(getDelay(gate.shape()));
            gate.onInputChange();
        }
    }
    
    @Override
    public void scheduledTick(SimpleGatePart gate) {
        int inputMask = inputMask(gate.shape());
        int outputMask = outputMask(gate.shape());
        int oldOutput = gate.state&outputMask;
        int newOutput = getOutput(getInput(gate, inputMask));
        if(oldOutput != newOutput) {
            gate.setState(gate.state & inputMask | newOutput);
            gate.onOutputChange(outputMask);
        }
        onChange(gate);
    }
}
