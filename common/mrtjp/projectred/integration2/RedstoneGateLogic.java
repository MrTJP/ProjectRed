package mrtjp.projectred.integration2;

import mrtjp.projectred.transmission.IConnectable;
import mrtjp.projectred.transmission.IRedwirePart;

public abstract class RedstoneGateLogic<PartType extends RedstoneGatePart> extends GateLogic<PartType>
{
    public abstract boolean getOutput(PartType gate, int r);
    
    @Override
    public boolean canConnectTo(PartType gate, IConnectable wire, int r) {
        return wire instanceof IRedwirePart && canConnect(gate, r);
    }

    public boolean canConnect(PartType gate, int r) {
        if(r == 0)
            return true;
        
        return canConnect(gate.shape(), r);
    }

    public boolean canConnect(int shape, int r) {
        return (inputMask(shape) & 1<<(r-1)) != 0;
    }
    
    public int inputMask(int shape) {
        return 0xE;
    }
    
    public int getInput(PartType gate, int inputMask) {
        int input = 0;
        for(int r = 1; r < 4; r++)
            if((inputMask & 1<<r) != 0 && gate.getRedwireInput(r) > 0)
                input |= 1<<r;
        
        return input;
    }
    
    public abstract int getOutput(int input);
}
