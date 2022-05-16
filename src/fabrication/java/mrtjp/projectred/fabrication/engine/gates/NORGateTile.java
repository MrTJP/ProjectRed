package mrtjp.projectred.fabrication.engine.gates;

import mrtjp.fengine.simulate.ICGate;
import mrtjp.fengine.simulate.ICSimulation;
import mrtjp.projectred.fabrication.engine.ICTileType;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.RenderGate;

public class NORGateTile extends SimpleGateTile {

    public NORGateTile() {
        super(ICTileType.NOR_GATE, RenderGate.getRenderIndex(GateType.NOR));
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
        return new NORGate();
    }

    public static class NORGate implements ICGate {

        @Override
        public void compute(ICSimulation ic, int[] inputs, int[] outputs) {
            for (int i = 0; i < inputs.length; i++) {
                if (ic.getRegByteVal(inputs[i]) != 0) {
                    ic.queueRegByteVal(outputs[0], (byte) 0);
                    return;
                }
            }
            ic.queueRegByteVal(outputs[0], (byte) 1);
        }
    }
}
