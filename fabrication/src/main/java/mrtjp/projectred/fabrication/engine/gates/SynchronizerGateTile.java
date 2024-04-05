package mrtjp.projectred.fabrication.engine.gates;

import mrtjp.fengine.simulate.ICGate;
import mrtjp.fengine.simulate.ICSimulation;

import java.util.ArrayList;
import java.util.List;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.LOGGER;
import static mrtjp.projectred.fabrication.engine.PRFabricationEngine.REG_TIME;

public class SynchronizerGateTile extends TimedStateGateTile {

    public SynchronizerGateTile() {
        super(ICGateTileType.SYNCHRONIZER);
    }

    //region RedstoneGateTile overrides
    @Override
    protected int redstoneInputMask() {
        return 0xE;
    }

    @Override
    protected int redstoneOutputMask() {
        return 0x1;
    }
    //endregion

    //region IGateRenderData overrides
    @Override
    public int state2() {
        // Note: this transforms internal state register to how Syncrhonizer Gate stores its state.
        int s2 = 0;
        int state = getState2() & 0xF;
        if (state == 2 || state == 4) s2 |= 1; // right
        if (state == 3 || state == 4) s2 |= 2; // left
        return s2;
    }
    //endregion

    @Override
    protected void collectGate(Collector collector, int gateId, int[] inputRegisters, int[] outputRegisters) {

        List<Integer> inputRegs = new ArrayList<>();
        List<Integer> outputRegs = new ArrayList<>();

        inputRegs.add(stateReg);            // 0 state
        inputRegs.add(inputRegisters[1]);   // 1 input right
        inputRegs.add(inputRegisters[2]);   // 2 input bottom
        inputRegs.add(inputRegisters[3]);   // 3 input left
        for (int i = 0; i < 8; i++) {
            inputRegs.add(timeRegs[i]);     // 4-11 time regs
        }
        for (int i = 0; i < 8; i++) {
            inputRegs.add(REG_TIME[i]);     // 12-19 sys time regs
        }

        outputRegs.add(stateReg);           // 0 state
        outputRegs.add(outputRegisters[0]); // 1 output
        for (int i = 0; i < 8; i++) {
            outputRegs.add(timeRegs[i]);    // 2-9 time regs
        }

        collector.addGate(gateId, new SynchronizerGate(), inputRegs, outputRegs);
    }

    public static class SynchronizerGate implements ICGate {

        private static byte readState(ICSimulation ic, int[] inputs) { return ic.getRegByteVal(inputs[0]); }
        private static byte readInputMask(ICSimulation ic, int[] inputs) {
            int mask = 0;
            if (ic.getRegByteVal(inputs[1]) != 0) mask |= 2;
            if (ic.getRegByteVal(inputs[2]) != 0) mask |= 4;
            if (ic.getRegByteVal(inputs[3]) != 0) mask |= 8;
            return (byte) mask;
        }
        private static long readSchedTime(ICSimulation ic, int[] inputs) { return ic.getRegLongVal(inputs, 4); }
        private static long readSysTime(ICSimulation ic, int[] inputs) { return ic.getRegLongVal(inputs, 12); }
        private static void writeState(ICSimulation ic, int[] outputs, byte state) { ic.queueRegByteVal(outputs[0], state); }
        private static void writeOutput(ICSimulation ic, int[] outputs, byte output) { ic.queueRegByteVal(outputs[1], output); }
        private static void writeSchedTime(ICSimulation ic, int[] outputs, long time) { ic.queueRegLongVal(outputs, 2, time); }

        @Override
        public void compute(ICSimulation ic, int[] inputs, int[] outputs) {

            int stateVal = readState(ic, inputs); // Lower nybble = state, upper nybble = prev input mask
            int state = stateVal & 0xF;
            int prevInputMask = (stateVal >> 4) & 0xF;

            int inputMask = readInputMask(ic, inputs);
            int highMask = inputMask & ~prevInputMask;

            switch (state) {
                case 0 -> { // Initial state
                    writeState(ic, outputs, (byte) 1);
                    writeOutput(ic, outputs, (byte) 0);
                    writeSchedTime(ic, outputs, -1);
                }

                case 1 -> { // Low state
                    if ((inputMask & 0x4) != 0 || highMask == 0) { // If reset input high
                        writeState(ic, outputs, (byte) (inputMask << 4 | 1)); // stay in low state

                    } else switch (highMask) {
                        case 0x2 -> {
                            writeState(ic, outputs, (byte)(inputMask << 4 | 2)); // enter R state
                        }
                        case 0x8 -> {
                            writeState(ic, outputs, (byte)(inputMask << 4 | 3)); // state L state
                        }
                        case 0xA -> {
                            writeState(ic, outputs, (byte)(inputMask << 4 | 4)); // enter tick state
                            writeOutput(ic, outputs, (byte) 1);
                            writeSchedTime(ic, outputs, readSysTime(ic, inputs) + 2);
                        }
                    }
                }
                case 2 -> { // R state
                    if ((inputMask & 0x4) != 0) { // If reset input high
                        writeState(ic, outputs, (byte) (inputMask << 4 | 1)); // enter low state

                    } else switch (highMask) {
                        case 0x8, 0xA -> {
                            writeState(ic, outputs, (byte)(inputMask << 4 | 4)); // enter tick state
                            writeOutput(ic, outputs, (byte) 1);
                            writeSchedTime(ic, outputs, readSysTime(ic, inputs) + 2);
                        }
                        default -> {
                            writeState(ic, outputs, (byte)(inputMask << 4 | 2)); // stay in R state
                        }
                    }
                }
                case 3 -> { // L state
                    if ((inputMask & 0x4) != 0) { // If reset input high
                        writeState(ic, outputs, (byte) (inputMask << 4 | 1)); // stay in low state

                    } else switch (highMask) {
                        case 0x2, 0xA -> {
                            writeState(ic, outputs, (byte)(inputMask << 4 | 4)); // enter tick state
                            writeOutput(ic, outputs, (byte) 1);
                            writeSchedTime(ic, outputs, readSysTime(ic, inputs) + 2);
                        }
                        default -> {
                            writeState(ic, outputs, (byte)(inputMask << 4 | 3)); // stay in R state
                        }
                    }
                }
                case 4 -> { // Tick state
                    if (readSysTime(ic, inputs) >= readSchedTime(ic, inputs)) { // if timer expired
                        writeState(ic, outputs, (byte) (inputMask << 4 | 1)); // low state
                        writeOutput(ic, outputs, (byte) 0); // set output low
                        writeSchedTime(ic, outputs, -1); // disable timer
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
