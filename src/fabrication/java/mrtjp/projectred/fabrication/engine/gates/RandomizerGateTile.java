package mrtjp.projectred.fabrication.engine.gates;

import mrtjp.fengine.simulate.ICGate;
import mrtjp.fengine.simulate.ICSimulation;
import mrtjp.projectred.fabrication.engine.ICTileType;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.RenderGate;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import static mrtjp.projectred.ProjectRedFabrication.LOGGER;
import static mrtjp.projectred.fabrication.engine.PRFabricationEngine.*;

public class RandomizerGateTile extends TimedStateGateTile {

    public RandomizerGateTile() {
        super(ICTileType.RANDOMIZER_GATE, RenderGate.getRenderIndex(GateType.RANDOMIZER));
    }

    //region GateTile overrides

    //TODO interaction zones

    //endregion

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
        return 0xB;
    }

    //endregion

    @Override
    protected void collectGate(Collector collector, int gateId, int[] inputRegisters, int[] outputRegisters) {

        List<Integer> inputRegs = new ArrayList<>();
        List<Integer> outputRegs = new ArrayList<>();

        inputRegs.add(stateReg);
        inputRegs.add(timeReg3);
        inputRegs.add(timeReg2);
        inputRegs.add(timeReg1);
        inputRegs.add(timeReg0);
        inputRegs.add(inputRegisters[2]);

        inputRegs.add(REG_TIME_3);
        inputRegs.add(REG_TIME_2);
        inputRegs.add(REG_TIME_1);
        inputRegs.add(REG_TIME_0);

        outputRegs.add(stateReg);
        outputRegs.add(timeReg3);
        outputRegs.add(timeReg2);
        outputRegs.add(timeReg1);
        outputRegs.add(timeReg0);
        outputRegs.add(outputRegisters[3] != -1 ? outputRegisters[3] : REG_ZERO);
        outputRegs.add(outputRegisters[0] != -1 ? outputRegisters[0] : REG_ZERO);
        outputRegs.add(outputRegisters[1] != -1 ? outputRegisters[1] : REG_ZERO);

        collector.addGate(gateId, new RandomizerGateTile.RandomizerGate(), inputRegs, outputRegs);
    }

    public static class RandomizerGate implements ICGate {

        private static final Random RAND = new Random();

        private static byte readState(ICSimulation ic, int[] inputs) { return ic.getRegByteVal(inputs[0]); }
        private static long readSchedTime(ICSimulation ic, int[] inputs) { return ic.getRegLongVal(inputs[1], inputs[2], inputs[3], inputs[4]); }
        private static byte readInput(ICSimulation ic, int[] inputs) { return ic.getRegByteVal(inputs[5]); }
        private static long readSysTime(ICSimulation ic, int[] inputs) { return ic.getRegLongVal(inputs[6], inputs[7], inputs[8], inputs[9]); }

        private static void writeState(ICSimulation ic, int[] outputs, byte state) { ic.queueRegByteVal(outputs[0], state); }
        private static void writeSchedTime(ICSimulation ic, int[] outputs, long time) { ic.queueRegLongVal(outputs[1], outputs[2], outputs[3], outputs[4], time); }
        private static void writeOutMask(ICSimulation ic, int[] outputs, int mask) {
            ic.queueRegByteVal(outputs[5], (byte) ((mask & 1) != 0 ? 1 : 0));
            ic.queueRegByteVal(outputs[6], (byte) ((mask & 2) != 0 ? 1 : 0));
            ic.queueRegByteVal(outputs[7], (byte) ((mask & 4) != 0 ? 1 : 0));
        }

        @Override
        public void compute(ICSimulation ic, int[] inputs, int[] outputs) {

            switch (readState(ic, inputs)) {
                case 0: { // Waiting for high input
                    if (readInput(ic, inputs) != 0) { // if input is high
                        writeState(ic, outputs, (byte) 1); // set state to 1
                        writeOutMask(ic, outputs, RAND.nextInt(8)); // set outputs to random state
                        writeSchedTime(ic, outputs, readSysTime(ic, inputs) + 2); // set scheduled time to t + 2
                    }
                    break;
                }
                case 1: { // Wait for timer state
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
                    break;
                }
                default:
                    LOGGER.error("Invalid state: " + readState(ic, inputs));
                    writeState(ic, outputs, (byte) 0); // go to state 0
            }
        }
    }
}
