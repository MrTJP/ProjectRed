package mrtjp.projectred.fabrication.engine.gates;

import mrtjp.fengine.simulate.ICGate;
import mrtjp.fengine.simulate.ICSimulation;
import mrtjp.projectred.fabrication.engine.ICTileType;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.RenderGate;

public class ORGateTile extends SimpleGateTile {

    public ORGateTile() {
        super(ICTileType.OR_GATE, RenderGate.getRenderIndex(GateType.OR));
    }

    @Override
    protected int redstoneOutputMask() {
        return 1;
    }

    @Override
    protected int redstoneInputMask() {
        return ~getShape() << 1 & 0xE;
    }

    @Override
    protected int getDeadSides() {
        return 3;
    }

    @Override
    public ICGate createGate() {
        return new ORGate();
    }

    public static class ORGate implements ICGate {

        @Override
        public void compute(ICSimulation ic, int[] inputs, int[] outputs) {
            for (int i = 0; i < inputs.length; i++) {
                if (ic.getRegByteVal(inputs[i]) != 0) {
                    ic.queueRegByteVal(outputs[0], (byte) 1);
                    return;
                }
            }
            ic.queueRegByteVal(outputs[0], (byte) 0);
        }
    }
}
