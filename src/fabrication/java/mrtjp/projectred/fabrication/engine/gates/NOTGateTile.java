package mrtjp.projectred.fabrication.engine.gates;

import mrtjp.fengine.simulate.ICGate;
import mrtjp.fengine.simulate.ICSimulation;
import mrtjp.projectred.fabrication.engine.ICTileType;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.RenderGate;

public class NOTGateTile extends SimpleGateTile {

    public NOTGateTile() {
        super(ICTileType.NOT_GATE, RenderGate.getRenderIndex(GateType.NOT));
    }

    @Override
    protected int redstoneOutputMask() {
        return ~((getShape() & 1) << 1 | (getShape() & 2) >> 1 | (getShape() & 4) << 1) & 0xB;
    }

    @Override
    protected int redstoneInputMask() {
        return 4;
    }

    @Override
    protected int getDeadSides() {
        return 3;
    }

    @Override
    public ICGate createGate() {
        return new NOTGate();
    }

    public static class NOTGate implements ICGate {

        @Override
        public void compute(ICSimulation ic, int[] inputs, int[] outputs) {
            boolean isHigh = ic.getRegByteVal(inputs[0]) != 0;
            for (int i = 0; i < outputs.length; i++) {
                ic.queueRegByteVal(outputs[i], isHigh ? 0 : (byte) 1);
            }
        }
    }
}
