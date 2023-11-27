package mrtjp.projectred.fabrication.engine.gates;

import mrtjp.fengine.simulate.ICGate;
import mrtjp.fengine.simulate.ICSimulation;

public class BufferGateTile extends SimpleGateTile {

    public BufferGateTile() {
        super(ICGateTileType.BUFFER);
    }

    @Override
    protected int redstoneOutputMask() {
        return ~((getShape() & 1) << 1 | (getShape() & 2) << 2) & 0xB;
    }

    @Override
    protected int redstoneInputMask() {
        return 4;
    }

    @Override
    protected int getDeadSides() {
        return 2;
    }

    @Override
    protected int getMaxDeadSides() {
        return 2;
    }

    @Override
    public ICGate createGate() {
        return new BufferGate();
    }

    public static class BufferGate implements ICGate {

        @Override
        public void compute(ICSimulation ic, int[] inputs, int[] outputs) {
            for (int i = 0; i < outputs.length; i++)
                ic.queueRegByteVal(outputs[i], ic.getRegByteVal(inputs[0]) != 0 ? (byte) 1 : (byte) 0);
        }
    }
}
