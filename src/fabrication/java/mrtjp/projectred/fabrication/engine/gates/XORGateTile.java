package mrtjp.projectred.fabrication.engine.gates;

import mrtjp.fengine.simulate.ICGate;
import mrtjp.fengine.simulate.ICSimulation;
import mrtjp.projectred.fabrication.engine.ICTileType;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.RenderGate;

public class XORGateTile extends SimpleGateTile {

    public XORGateTile() {
        super(ICTileType.XOR_GATE, RenderGate.getRenderIndex(GateType.XOR));
    }

    @Override
    protected int redstoneOutputMask() {
        return 1;
    }

    @Override
    protected int redstoneInputMask() {
        return 10;
    }

    @Override
    public ICGate createGate() {
        return new XORGate();
    }

    public static class XORGate implements ICGate {

        @Override
        public void compute(ICSimulation ic, int[] inputs, int[] outputs) {
            boolean in0 = ic.getRegByteVal(inputs[0]) != 0;
            boolean in1 = ic.getRegByteVal(inputs[1]) != 0;
            ic.queueRegByteVal(outputs[0], in0 != in1 ? (byte) 1 : (byte) 0);
        }
    }
}
