package mrtjp.projectred.fabrication.engine.gates;

import mrtjp.fengine.simulate.ICGate;
import mrtjp.fengine.simulate.ICSimulation;

import java.util.ArrayList;
import java.util.List;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.LOGGER;
import static mrtjp.projectred.fabrication.engine.PRFabricationEngine.REG_TIME;

public class TimerGateTile extends RedstoneTimerGateTile {

    public TimerGateTile() {
        super(ICGateTileType.TIMER);
    }

    //region RedstoneGateTile overrides
    @Override
    protected int redstoneInputMask() {
        // TODO support inout connections
//        return 0xE;
        return 0x4;
    }

    @Override
    protected int redstoneOutputMask() {
        return 0xB;
    }
    //endregion

    //region IGateRenderData overrides
    public boolean isPointerStarted() {
        if (!getEditor().getStateMachine().isSimulating()) return false;
        long simTime = getEditor().getStateMachine().getSimSystemTime();
        // Upper bound is checked because internally, pointer goes 2 ticks above the max time (for high state timing)
        return getState2() == 2 && simTime < pointerStartTime + pointerMax;
    }
    //endregion

    @Override
    protected void collectGate(Collector collector, int gateId, int[] inputRegisters, int[] outputRegisters) {
        List<Integer> inputRegs = new ArrayList<>();
        List<Integer> outputRegs = new ArrayList<>();

        inputRegs.add(stateReg);                // 0: state
        inputRegs.add(inputRegisters[2]);       // 1: input
        for (int i = 0; i < 8; i++) {           // 2-9: start time
            inputRegs.add(timeRegs[i]);
        }
        for (int i = 0; i < 8; i++) {           // 10-17: sysTime
            inputRegs.add(REG_TIME[i]);
        }

        outputRegs.add(stateReg);               // 0: state
        outputRegs.add(outputRegisters[0]);     // 1: output top
        outputRegs.add(outputRegisters[1]);     // 2: output right
        outputRegs.add(outputRegisters[3]);     // 3: output left
        for (int i = 0; i < 8; i++) {           // 4-11: start time
            outputRegs.add(timeRegs[i]);
        }

        collector.addGate(gateId, new TimerGate(pointerMax), inputRegs, outputRegs);
    }

    public static class TimerGate implements ICGate {

        private final long pointerMax;

        public TimerGate(long pointerMax) {
            this.pointerMax = pointerMax;
        }

        private static byte readState(ICSimulation ic, int[] inputs) { return ic.getRegByteVal(inputs[0]); }
        private static byte readInput(ICSimulation ic, int[] inputs) { return ic.getRegByteVal(inputs[1]); }
        private static long readStartTime(ICSimulation ic, int[] inputs) { return ic.getRegLongVal(inputs, 2); }
        private static long readSysTime(ICSimulation ic, int[] inputs) { return ic.getRegLongVal(inputs, 10); }
        private static void writeState(ICSimulation ic, int[] outputs, byte state) { ic.queueRegByteVal(outputs[0], state); }
        private static void writeOutput(ICSimulation ic, int[] outputs, byte output) {
            ic.queueRegByteVal(outputs[1], output);
            ic.queueRegByteVal(outputs[2], output);
            ic.queueRegByteVal(outputs[3], output);
        }
        private static void writeStartTime(ICSimulation ic, int[] outputs, long time) { ic.queueRegLongVal(outputs, 4, time); }

        private static void enterHaltState(ICSimulation ic, int[] outputs) {
            writeState(ic, outputs, (byte) 1); // state 1
            writeOutput(ic, outputs, (byte) 0); // output low
            writeStartTime(ic, outputs, -1); // stop timer
        }

        private static void enterCountingState(ICSimulation ic, int[] inputs, int[] outputs) {
            writeState(ic, outputs, (byte) 2); // state 2
            writeOutput(ic, outputs, (byte) 0); // output low
            writeStartTime(ic, outputs, readSysTime(ic, inputs)); // write start time
        }

        private static void enterTickState(ICSimulation ic, int[] outputs) {
            writeState(ic, outputs, (byte) 3); // state 3
            writeOutput(ic, outputs, (byte) 1); // output high
        }

        @Override
        public void compute(ICSimulation ic, int[] inputs, int[] outputs) {
            int stateVal = readState(ic, inputs);
            switch (stateVal) {
                case 0 -> { // Initial state
                    enterHaltState(ic, outputs);
                }
                case 1 -> { // Halt state
                    if (readInput(ic, inputs) == 0) {
                        enterCountingState(ic, inputs, outputs);
                    }
                }
                case 2 -> { // Counting state
                    if (readInput(ic, inputs) != 0) {
                        enterHaltState(ic, outputs);
                    }
                    // If at max time
                    else if (readSysTime(ic, inputs) >= readStartTime(ic, inputs) + pointerMax) {
                        enterTickState(ic, outputs);
                    }
                }
                case 3 -> { // Ticking state
                    // If 2 ticks past max time
                    if (readSysTime(ic, inputs) >= readStartTime(ic, inputs) + pointerMax + 2) {
                        if (readInput(ic, inputs) != 0) {
                            enterHaltState(ic, outputs);
                        } else {
                            enterCountingState(ic, inputs, outputs);
                        }
                    }
                }
                default -> {
                    LOGGER.error("Invalid state: " + stateVal);
                    enterHaltState(ic, outputs);
                }
            }
        }
    }
}
