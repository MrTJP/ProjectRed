package mrtjp.projectred.integration;

import dan200.computer.api.IComputerAccess;
import dan200.computer.api.ILuaContext;
import dan200.computer.api.IPeripheral;

public class GatePeripheralPart extends GatePart implements IPeripheral {

    public GatePeripheralPart(EnumGate type) {
        super(type);
    }

    @Override
    public String[] getMethodNames() {
        if (getLogic() instanceof IPeripheral) {
            return ((IPeripheral)getLogic()).getMethodNames();
        }
        return null;
    }

    @Override
    public Object[] callMethod(IComputerAccess computer, ILuaContext context, int method, Object[] arguments) throws Exception {
        updateLogic(false, true);
        if (getLogic() instanceof IPeripheral) {
            return ((IPeripheral)getLogic()).callMethod(computer, context, method, arguments);
        }
        return null;
    }

    @Override
    public boolean canAttachToSide(int attachSide) {
        if (getLogic() instanceof IPeripheral) {
            return ((IPeripheral)getLogic()).canAttachToSide(Rotator.absoluteToRelative(getSide(), getFront(), attachSide));
        }
        return false;
    }

    @Override
    public void attach(IComputerAccess computer) {
        updateLogic(false, true);
        if (getLogic() instanceof IPeripheral) {
            ((IPeripheral)getLogic()).attach(computer);
        }
    }

    @Override
    public void detach(IComputerAccess computer) {
        updateLogic(false, true);
        if (getLogic() instanceof IPeripheral) {
            ((IPeripheral)getLogic()).detach(computer);
        }
    }
}
