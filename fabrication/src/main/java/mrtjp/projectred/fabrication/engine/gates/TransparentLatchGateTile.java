package mrtjp.projectred.fabrication.engine.gates;

import mrtjp.fengine.simulate.ICGate;
import mrtjp.fengine.simulate.ICSimulation;

import java.util.ArrayList;
import java.util.List;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.LOGGER;

public class TransparentLatchGateTile extends InternalStateGateTile {

    public TransparentLatchGateTile() {
        super(ICGateTileType.TRANSPARENT_LATCH);
    }

    //region GateTile overrides

    //TODO interaction zones

    //endregion

    //region RedstoneGateTile overrides

    @Override
    protected boolean cycleShape() {
        setShape((getShape() + 1) % 2);
        return true;
    }

    @Override
    protected int redstoneInputMask() {
        return getShape() == 0 ? 0xC : 0x6;
    }

    @Override
    protected int redstoneOutputMask() {
        return getShape() == 0 ? 0x3 : 0x9;
    }

    //endregion

    @Override
    protected void collectGate(Collector collector, int gateId, int[] inputRegisters, int[] outputRegisters) {

        List<Integer> inputRegs = new ArrayList<>();
        List<Integer> outputRegs = new ArrayList<>();

        inputRegs.add(stateReg);
        inputRegs.add(inputRegisters[getShape() == 0 ? 3 : 1]); //dataIn
        inputRegs.add(inputRegisters[2]); //writeEnable

        outputRegs.add(stateReg);
        outputRegs.add(outputRegisters[0]); //out1
        outputRegs.add(outputRegisters[getShape() == 0 ? 1 : 3]); //out2

        collector.addGate(gateId, new TransparentLatchGate(), inputRegs, outputRegs);
    }

    public static class TransparentLatchGate implements ICGate {

        private static byte readState(ICSimulation ic, int[] inputs) { return ic.getRegByteVal(inputs[0]); }

        private static byte readData(ICSimulation ic, int[] inputs) {
            return ic.getRegByteVal(inputs[1]);
        }

        private static boolean writeEnabled(ICSimulation ic, int[] inputs) {
            return ic.getRegByteVal(inputs[2]) != 0;
        }

        private static void writeState(ICSimulation ic, int[] outputs, byte state) { ic.queueRegByteVal(outputs[0], state); }

        private static void writeData(ICSimulation ic, int[] outputs, byte data) {
            ic.queueRegByteVal(outputs[1], data);
            ic.queueRegByteVal(outputs[2], data);
        }

        @Override
        public void compute(ICSimulation ic, int[] inputs, int[] outputs) {

            switch (readState(ic, inputs)) {
                case 0: { // Initial state
                    if (writeEnabled(ic, inputs)) {
                        writeState(ic, outputs, (byte) 2); // Enter write state
                    } else {
                        writeState(ic, outputs, (byte) 1); // Enter lock state
                    }
                    break;
                }

                case 1: { // Lock state
                    if (writeEnabled(ic, inputs)) {
                        writeState(ic, outputs, (byte) 2); // Enter write state
                    }
                    break;
                }

                case 2: { // Write state
                    if (writeEnabled(ic, inputs)) {
                        writeData(ic, outputs, readData(ic, inputs));
                    } else {
                        writeState(ic, outputs, (byte) 1); // Enter lock state
                    }
                    break;
                }

                default:
                    LOGGER.error("Invalid state: " + readState(ic, inputs));
                    writeState(ic, outputs, (byte) 0);
            }
        }
    }
}
