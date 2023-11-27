package mrtjp.projectred.fabrication.engine.gates;

import mrtjp.fengine.simulate.ICGate;
import mrtjp.fengine.simulate.ICSimulation;

public class MultiplexerGateTile extends SimpleGateTile {

    public MultiplexerGateTile() {
        super(ICGateTileType.MULTIPLEXER);
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
