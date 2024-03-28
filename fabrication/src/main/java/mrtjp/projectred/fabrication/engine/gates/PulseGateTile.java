package mrtjp.projectred.fabrication.engine.gates;

import mrtjp.fengine.simulate.ICGate;
import mrtjp.fengine.simulate.ICSimulation;

import java.util.ArrayList;
import java.util.List;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.LOGGER;
import static mrtjp.projectred.fabrication.engine.PRFabricationEngine.REG_TIME;

public class PulseGateTile extends TimedStateGateTile {

    public PulseGateTile() {
        super(ICGateTileType.PULSE);
    }

    //region RedstoneGateTile overrides

    @Override
    protected boolean cycleShape() {
        return false;
    }

    @Override
    protected int redstoneInputMask() {
        return 4;
    }

    @Override
    protected int redstoneOutputMask() {
        return 1;
    }

    //endregion

    @Override
    protected void collectGate(Collector collector, int gateId, int[] inputRegisters, int[] outputRegisters) {

        List<Integer> inputRegs = new ArrayList<>();
        List<Integer> outputRegs = new ArrayList<>();

        inputRegs.add(stateReg);
        inputRegs.add(inputRegisters[2]);
        for (int i = 0; i < 8; i++) {
            inputRegs.add(timeRegs[i]);
        }
        for (int i = 0; i < 8; i++) {
            inputRegs.add(REG_TIME[i]);
        }

        outputRegs.add(stateReg);
        outputRegs.add(outputRegisters[0]);
        for (int i = 0; i < 8; i++) {
            outputRegs.add(timeRegs[i]);
        }

        collector.addGate(gateId, new PulseGate(), inputRegs, outputRegs);
    }

    public static class PulseGate implements ICGate {

        private static byte readState(ICSimulation ic, int[] inputs) { return ic.getRegByteVal(inputs[0]); }
        private static byte readInput(ICSimulation ic, int[] inputs) { return ic.getRegByteVal(inputs[1]); }
        private static long readSchedTime(ICSimulation ic, int[] inputs) { return ic.getRegLongVal(inputs, 2); }
        private static long readSysTime(ICSimulation ic, int[] inputs) { return ic.getRegLongVal(inputs, 10); }
        private static void writeState(ICSimulation ic, int[] outputs, byte state) { ic.queueRegByteVal(outputs[0], state); }
        private static void writeOutput(ICSimulation ic, int[] outputs, byte output) { ic.queueRegByteVal(outputs[1], output); }
        private static void writeSchedTime(ICSimulation ic, int[] outputs, long time) { ic.queueRegLongVal(outputs, 2, time); }

        @Override
        public void compute(ICSimulation ic, int[] inputs, int[] outputs) {

            int stateVal = readState(ic, inputs);
            switch (stateVal) {
                case 0 -> { // Waiting for high input
                    if (readInput(ic, inputs) != 0) { // if input is high
                        writeState(ic, outputs, (byte) 1); // go to state 1
                        writeOutput(ic, outputs, (byte) 1); // set output high
                        writeSchedTime(ic, outputs, readSysTime(ic, inputs) + 2); // set time to t + 2
                    }
                }
                case 1 -> { // Waiting for timer to expire
                    if (readSysTime(ic, inputs) >= readSchedTime(ic, inputs)) { // if timer expired
                        writeState(ic, outputs, (byte) (readInput(ic, inputs) == 0 ? 0 : 2)); // if input high, got to state 2, else state 0
                        writeOutput(ic, outputs, (byte) 0); // set output low
                        writeSchedTime(ic, outputs, -1); // disable timer
                    }
                }
                case 2 -> { // Waiting for low input state
                    if (readInput(ic, inputs) == 0) { // if input is low
                        writeState(ic, outputs, (byte) 0); // go to state 0
                    }
                }
                default -> {
                    LOGGER.error("Invalid state: " + stateVal);
                    writeState(ic, outputs, (byte) 0); // go to state 0
                }
            }
        }
    }
}
