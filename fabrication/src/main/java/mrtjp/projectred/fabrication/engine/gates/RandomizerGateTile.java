package mrtjp.projectred.fabrication.engine.gates;

import mrtjp.fengine.simulate.ICGate;
import mrtjp.fengine.simulate.ICSimulation;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.LOGGER;
import static mrtjp.projectred.fabrication.engine.PRFabricationEngine.REG_TIME;
import static mrtjp.projectred.fabrication.engine.PRFabricationEngine.REG_ZERO;

public class RandomizerGateTile extends SimpleTimedStateGateTile {

    public RandomizerGateTile() {
        super(ICGateTileType.RANDOMIZER);
    }

    //region SimpleTimedStateGateTile overrides
    @Override
    protected int interactMask() {
        return 0xB;
    }

    @Override
    protected int getDeadSides() {
        return 3;
    }

    @Override
    protected int rotationToDeadSideBit(int r) {
        return switch (r) {
            case 0 -> 0x2;
            case 1 -> 0x1;
            case 3 -> 0x4;
            default -> 0;
        };
    }
    //endregion

    //region RedstoneGateTile overrides
    @Override
    protected int redstoneInputMask() {
        return 4;
    }

    @Override
    protected int redstoneOutputMask() {
        return ~((getShape() & 1) << 1 | (getShape() & 2) >> 1 | (getShape() & 4) << 1) & 0xB;
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
        outputRegs.add(outputRegisters[3] != -1 ? outputRegisters[3] : REG_ZERO);
        outputRegs.add(outputRegisters[0] != -1 ? outputRegisters[0] : REG_ZERO);
        outputRegs.add(outputRegisters[1] != -1 ? outputRegisters[1] : REG_ZERO);
        for (int i = 0; i < 8; i++) {
            outputRegs.add(timeRegs[i]);
        }

        collector.addGate(gateId, new RandomizerGateTile.RandomizerGate(), inputRegs, outputRegs);
    }

    public static class RandomizerGate implements ICGate {

        private static final Random RAND = new Random();

        private static byte readState(ICSimulation ic, int[] inputs) { return ic.getRegByteVal(inputs[0]); }
        private static byte readInput(ICSimulation ic, int[] inputs) { return ic.getRegByteVal(inputs[1]); }
        private static long readSchedTime(ICSimulation ic, int[] inputs) { return ic.getRegLongVal(inputs, 2); }
        private static long readSysTime(ICSimulation ic, int[] inputs) { return ic.getRegLongVal(inputs, 10); }
        private static void writeState(ICSimulation ic, int[] outputs, byte state) { ic.queueRegByteVal(outputs[0], state); }
        private static void writeOutMask(ICSimulation ic, int[] outputs, int mask) {
            ic.queueRegByteVal(outputs[1], (byte) ((mask & 1) != 0 ? 1 : 0));
            ic.queueRegByteVal(outputs[2], (byte) ((mask & 2) != 0 ? 1 : 0));
            ic.queueRegByteVal(outputs[3], (byte) ((mask & 4) != 0 ? 1 : 0));
        }
        private static void writeSchedTime(ICSimulation ic, int[] outputs, long time) { ic.queueRegLongVal(outputs, 4, time); }

        @Override
        public void compute(ICSimulation ic, int[] inputs, int[] outputs) {

            int stateVal = readState(ic, inputs);
            switch (stateVal) {
                case 0 -> { // Waiting for high input
                    if (readInput(ic, inputs) != 0) { // if input is high
                        writeState(ic, outputs, (byte) 1); // set state to 1
                        writeOutMask(ic, outputs, RAND.nextInt(8)); // set outputs to random state
                        writeSchedTime(ic, outputs, readSysTime(ic, inputs) + 2); // set scheduled time to t + 2
                    }
                }
                case 1 -> { // Wait for timer state
                    if (readSysTime(ic, inputs) >= readSchedTime(ic, inputs)) { // if time is up
                        if (readInput(ic, inputs) != 0) { // if input is high
                            // stay in state 1
                            writeOutMask(ic, outputs, RAND.nextInt(8)); // set outputs to random state
                            writeSchedTime(ic, outputs, readSysTime(ic, inputs) + 2); // set scheduled time to t + 2
                        } else {
                            writeState(ic, outputs, (byte) 0); // set state to 0
                            writeOutMask(ic, outputs, 0); // set outputs to 0
                        }
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
