package mrtjp.projectred.integration2;

import mrtjp.projectred.transmission.IConnectable;

public abstract class GateLogic<PartType extends GatePart>
{
    public abstract boolean canConnectTo(PartType gate, IConnectable part, int r);
    
    public abstract int cycleShape(int shape);
    
    public abstract void onChange(PartType gate);
    
    public abstract void scheduledTick(PartType gate);
}
