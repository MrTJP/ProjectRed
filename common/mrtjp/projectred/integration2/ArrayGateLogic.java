package mrtjp.projectred.integration2;

import mrtjp.projectred.transmission.IConnectable;
import mrtjp.projectred.transmission.IRedwireEmitter;

public abstract class ArrayGateLogic extends GateLogic<ArrayGatePart>
{
    public static ArrayGateLogic[] logic = new ArrayGateLogic[]{
            new Null(),
            new Invert(),
            new Buffer()
        };
    
    @Override
    public boolean canConnectTo(ArrayGatePart gate, IConnectable wire, int r) {
        return wire instanceof IRedwireEmitter;
    }
    
    @Override
    public void onChange(ArrayGatePart gate) {
        boolean oldSignal = (gate.state&1) != 0;
        boolean newSignal = gate.signal1 != 0;
        
        if(oldSignal != newSignal) {
            gate.setState(gate.state & 2 | (newSignal ? 1 : 0));
            gate.scheduleTick(2);
            gate.tile().markDirty();//save
        }
    }
    
    @Override
    public void scheduledTick(ArrayGatePart gate) {
        boolean input = (gate.state&1) != 0;
        boolean oldOutput = (gate.state&2) != 0;
        boolean newOutput = !input;
        
        if(oldOutput != newOutput) {
            gate.setState(gate.state & 1 | (newOutput ? 2 : 0));
            gate.sendStateUpdate();
            gate.tile().markDirty();
            gate.onChange();
        }
    }

    public boolean canCross() {
        return false;
    }
    
    public abstract boolean powerUp(int state);
    
    public static class Null extends ArrayGateLogic
    {
        @Override
        public boolean powerUp(int state) {
            return false;
        }
        
        @Override
        public boolean canCross() {
            return true;
        }
    }
    
    public static class Invert extends ArrayGateLogic
    {
        @Override
        public boolean powerUp(int state) {
            return (state&2) != 0;
        }
    }
    
    public static class Buffer extends ArrayGateLogic
    {
        @Override
        public boolean powerUp(int state) {
            return (state&2) == 0;
        }
    }
}
