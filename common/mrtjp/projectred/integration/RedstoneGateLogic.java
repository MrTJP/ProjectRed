package mrtjp.projectred.integration;

import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.transmission.IRedwireEmitter;

public abstract class RedstoneGateLogic<PartType extends RedstoneGatePart> extends GateLogic<PartType>
{
    public abstract int getOutput(PartType gate, int r);
    
    @Override
    public boolean canConnectTo(PartType gate, IConnectable wire, int r) {
        return wire instanceof IRedwireEmitter && canConnect(gate, r);
    }
    
    public boolean canConnect(PartType gate, int r) {
        return canConnect(gate.shape(), r);
    }

    public boolean canConnect(int shape, int r) {
        return ((inputMask(shape)|outputMask(shape)) & 1<<r) != 0;
    }
    
    public int inputMask(int shape) {
        return 0xE;
    }
    
    public int outputMask(int shape) {
        return 1;
    }
    
    public int getInput(PartType gate, int inputMask) {
        int input = 0;
        for(int r = 0; r < 4; r++)
            if((inputMask & 1<<r) != 0 && gate.getRedstoneInput(r) > 0)
                input |= 1<<r;
        
        return input;
    }
    
    public int calcOutput(PartType gate, int input) {
        return calcOutput(input);
    }
    
    public int calcOutput(int input) {
        return 0;
    }

    public boolean requireStrongInput(int r) {
        return false;
    }
}
