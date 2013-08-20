package mrtjp.projectred.integration2;

public abstract class SimpleGateLogic extends RedstoneGateLogic<SimpleGatePart>
{
    public static SimpleGateLogic[] instances = new SimpleGateLogic[]{
            new OR()
        };
    
    public static class OR extends SimpleGateLogic
    {
        @Override
        public int inputMask(int shape) {
            return ~shape;
        }
        
        @Override
        public int maxShape() {
            return 7;
        }
        
        @Override
        public int getOutput(int input) {
            return input != 0 ? 1 : 0;
        }
    }
    
    public boolean getOutput(SimpleGatePart gate, int r) {
        return (gate.state & ~inputMask(gate.shape()) & 1<<r) != 0;
    }
    
    public int cycleShape(int shape) {
        return (shape+1)%maxShape();
    }
    
    public int maxShape() {
        return 1;
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
            gate.setState(gate.state & ~inputMask | newInput);
            gate.scheduleTick(getDelay(gate.shape()));
            
            gate.notifyInternalChange();
        }
    }
    
    @Override
    public void scheduledTick(SimpleGatePart gate) {
        int inputMask = inputMask(gate.shape());
        int oldOutput = gate.state&~inputMask;
        int newOutput = getOutput(getInput(gate, inputMask));
        if(oldOutput != newOutput) {
            gate.setState(gate.state & inputMask | newOutput);
            gate.notifyChange();
        }
        onChange(gate);
    }
}
