package mrtjp.projectred.fabrication.engine.gates;

import mrtjp.fengine.simulate.ICGate;
import mrtjp.fengine.simulate.ICSimulation;
import mrtjp.projectred.fabrication.engine.ICTileType;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.RenderGate;

public class MultiplexerGateTile extends SimpleGateTile {

    public MultiplexerGateTile() {
        super(ICTileType.MULTIPLEXER_GATE, RenderGate.getRenderIndex(GateType.MULTIPLEXER));
    }

    @Override
    protected int redstoneOutputMask() {
        return 1;
    }

    @Override
    protected int redstoneInputMask() {
        return 0xE;
    }

    @Override
    public ICGate createGate() {
        return new MultiplexerGate();
    }

    public static class MultiplexerGate implements ICGate {

        @Override
        public void compute(ICSimulation ic, int[] inputs, int[] outputs) {

            boolean in0 = ic.getRegByteVal(inputs[0]) != 0;
            boolean in1 = ic.getRegByteVal(inputs[1]) != 0;
            boolean in2 = ic.getRegByteVal(inputs[2]) != 0;

            ic.queueRegByteVal(outputs[0], in0 ?
                    (byte) (in1 ? 1 : 0) :
                    (byte) (in2 ? 1 : 0));
        }
    }
}
