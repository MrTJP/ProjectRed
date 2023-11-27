package mrtjp.projectred.fabrication.engine.gates;

import mrtjp.fengine.simulate.ICGate;
import mrtjp.fengine.simulate.ICSimulation;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.LOGGER;
import static mrtjp.projectred.fabrication.engine.PRFabricationEngine.REG_ZERO;

public class SRLatchGateTile extends InternalStateGateTile {

    public SRLatchGateTile() {
        super(ICGateTileType.SR_LATCH);
    }

    //region GateTile overrides

    //TODO interaction zones

    //endregion

    //region RedstoneGateTile overrides

    @Override
    protected boolean cycleShape() {
        setShape((getShape() + 1) % 4);
        return true;
    }

    @Override
    protected int redstoneInputMask() {
        return 0xA;
    }

    @Override
    protected int redstoneOutputMask() {
        return (getShape() >> 1) == 0 ? 0xF : 5;
    }

    //endregion

    @Override
    protected void collectGate(Collector collector, int gateId, int[] inputRegisters, int[] outputRegisters) {

        List<Integer> inputRegs = new ArrayList<>();
        List<Integer> outputRegs = new ArrayList<>();

        boolean reflected = (getShape() & 1) != 0;
        boolean backfeed = (getShape() & 2) == 0;

        inputRegs.add(stateReg);
        inputRegs.add(inputRegisters[reflected ? 3 : 1]); //inA
        inputRegs.add(inputRegisters[reflected ? 1 : 3]); //inB

        outputRegs.add(stateReg);
        outputRegs.add(outputRegisters[2]); //outA
        outputRegs.add(outputRegisters[0]); //outB
        outputRegs.add(backfeed ? outputRegisters[reflected ? 3 : 1] : REG_ZERO); //backfeedA
        outputRegs.add(backfeed ? outputRegisters[reflected ? 1 : 3] : REG_ZERO); //backfeedB

        collector.addGate(gateId, new SRLatchGate(), inputRegs, outputRegs);
    }

    public static class SRLatchGate implements ICGate {

        private static final Random RAND = new Random();

        private static byte readState(ICSimulation ic, int[] inputs) { return ic.getRegByteVal(inputs[0]); }

        private static int readInputMask(ICSimulation ic, int[] inputs) {
            int mask = 0;
            if (ic.getRegByteVal(inputs[1]) != 0) mask |= 1;
            if (ic.getRegByteVal(inputs[2]) != 0) mask |= 2;
            return mask;
        }

        private static void writeState(ICSimulation ic, int[] outputs, byte state) { ic.queueRegByteVal(outputs[0], state); }

        private static void writeOutputMask(ICSimulation ic, int[] outputs, int mask) {
            ic.queueRegByteVal(outputs[1], (byte) ((mask & 1) != 0 ? 1 : 0)); // outA
            ic.queueRegByteVal(outputs[3], (byte) ((mask & 1) != 0 ? 1 : 0)); // backfeedA
            ic.queueRegByteVal(outputs[2], (byte) ((mask & 2) != 0 ? 1 : 0)); // outB
            ic.queueRegByteVal(outputs[4], (byte) ((mask & 2) != 0 ? 1 : 0)); // backfeedB
        }

        private static void enterStateA(ICSimulation ic, int[] outputs) {
            writeState(ic, outputs, (byte) 1);
            writeOutputMask(ic, outputs, 0x1);
        }

        private static void enterStateB(ICSimulation ic, int[] outputs) {
            writeState(ic, outputs, (byte) 2);
            writeOutputMask(ic, outputs, 0x2);
        }

        private static void enterUndefState(ICSimulation ic, int[] outputs) {
            writeState(ic, outputs, (byte) 3);
            writeOutputMask(ic, outputs, 0x0);
        }

        @Override
        public void compute(ICSimulation ic, int[] inputs, int[] outputs) {
            int inputMask = readInputMask(ic, inputs);
            switch (readState(ic, inputs)) {
                case 0: {
                    if (inputMask == 2) {
                        enterStateB(ic, outputs);
                    } else if (inputMask == 3) {
                        enterUndefState(ic, outputs);
                    } else {
                        enterStateA(ic, outputs);
                    }
                }

                case 1: { // A state
                    if (inputMask == 2) {
                        enterStateB(ic, outputs);
                    } else if (inputMask == 3) {
                        enterUndefState(ic, outputs);
                    }
                    break;
                }

                case 2: { // B state
                    if (inputMask == 1) {
                        enterStateA(ic, outputs);
                    } else if (inputMask == 3) {
                        enterUndefState(ic, outputs);
                    }
                    break;
                }

                case 3: { // Undefined state
                    if (inputMask == 0) {
                        if (RAND.nextBoolean()) {
                            enterStateA(ic, outputs);
                        } else {
                            enterStateB(ic, outputs);
                        }
                    } else if (inputMask == 1) {
                        enterStateA(ic, outputs);
                    } else if (inputMask == 2) {
                        enterStateB(ic, outputs);
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
