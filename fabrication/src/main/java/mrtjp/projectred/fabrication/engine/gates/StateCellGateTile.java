package mrtjp.projectred.fabrication.engine.gates;

import mrtjp.fengine.simulate.ICGate;
import mrtjp.fengine.simulate.ICSimulation;

import java.util.ArrayList;
import java.util.List;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.LOGGER;
import static mrtjp.projectred.fabrication.engine.IRotatableICTile.flipMaskZ;
import static mrtjp.projectred.fabrication.engine.PRFabricationEngine.REG_TIME;

public class StateCellGateTile extends RedstoneTimerGateTile {

    public StateCellGateTile() {
        super(ICGateTileType.STATE_CELL);
    }

    //region GateTile overrides
    @Override
    protected boolean canReflect() {
        return true;
    }

    @Override
    protected void reflectAndSend() {
        configureShapeAndSend(getShape() == 0 ? 1 : 0);
    }
    //endregion

    //region RedstoneGateTile overrides
    @Override
    protected int redstoneOutputMask() {
        return getShape() == 0 ? 0x9 : flipMaskZ(0x9);
    }

    @Override
    protected int redstoneInputMask() {
        return getShape() == 0 ? 0x6 : flipMaskZ(0x6);
    }
    //endregion

    //region TimedStateGateTile overrides
    public boolean isPointerStarted() {
        if (!getEditor().getStateMachine().isSimulating()) return false;
        long simTime = getEditor().getStateMachine().getSimSystemTime();
        // Upper bound is checked because internally, pointer goes 2 ticks above the max time (for high state timing)
        return getState2() == 2 && simTime < pointerStartTime + pointerMax;
    }
    //endregion

    //region IGateRenderData overrides
    @Override
    public int state2() {
        int s2 = getState2();
        return s2 == 0 ? 0 : 1;
    }
    //endregion

    @Override
    protected void collectGate(Collector collector, int gateId, int[] inputRegisters, int[] outputRegisters) {
        List<Integer> inputRegs = new ArrayList<>();
        List<Integer> outputRegs = new ArrayList<>();

        inputRegs.add(stateReg);                                    // 0: state
        inputRegs.add(inputRegisters[getShape() == 0 ? 1 : 3]);     // 1: reset input
        inputRegs.add(inputRegisters[2]);                           // 2: start/reset input
        for (int i = 0; i < 8; i++) {                               // 3-10: start time
            inputRegs.add(timeRegs[i]);
        }
        for (int i = 0; i < 8; i++) {                               // 11-18: sysTime
            inputRegs.add(REG_TIME[i]);
        }

        outputRegs.add(stateReg);                                   // 0: state
        outputRegs.add(outputRegisters[0]);                         // 1: pulse output
        outputRegs.add(outputRegisters[getShape() == 0 ? 3 : 1]);   // 2: running output
        for (int i = 0; i < 8; i++) {                               // 3-10: start time
            outputRegs.add(timeRegs[i]);
        }

        collector.addGate(gateId, new StateCellGate(pointerMax), inputRegs, outputRegs);
    }

    public static class StateCellGate implements ICGate {

        private final long pointerMax;

        private static byte readState(ICSimulation ic, int[] inputs) { return ic.getRegByteVal(inputs[0]); }
        private static byte readInputMask(ICSimulation ic, int[] inputs) {
            int mask = 0;
            if (ic.getRegByteVal(inputs[1]) != 0) mask |= 0x2;
            if (ic.getRegByteVal(inputs[2]) != 0) mask |= 0x4;
            return (byte) mask;
        }
        private static long readStartTime(ICSimulation ic, int[] inputs) { return ic.getRegLongVal(inputs, 3); }
        private static long readSysTime(ICSimulation ic, int[] inputs) { return ic.getRegLongVal(inputs, 11); }
        private static void writeState(ICSimulation ic, int[] outputs, byte state) { ic.queueRegByteVal(outputs[0], state); }
        private static void writeOutputMask(ICSimulation ic, int[] outputs, byte oMask) {
            ic.queueRegByteVal(outputs[1], (oMask & 0x1) != 0 ? (byte) 1 : (byte) 0);
            ic.queueRegByteVal(outputs[2], (oMask & 0x8) != 0 ? (byte) 1 : (byte) 0);
        }
        private static void writeStartTime(ICSimulation ic, int[] outputs, long time) { ic.queueRegLongVal(outputs, 3, time); }

        public StateCellGate(long pointerMax) {
            this.pointerMax = pointerMax;
        }

        @Override
        public void compute(ICSimulation ic, int[] inputs, int[] outputs) {
            int stateVal = readState(ic, inputs);
            switch (stateVal) {
                case 0 -> { // inactive state
                    if ((readInputMask(ic, inputs) & 0x4) != 0) { // if start input high
                        writeState(ic, outputs, (byte) 1); // enter running hold state
                        writeOutputMask(ic, outputs, (byte) 0x8); // enable running output
                    }
                }

                case 1 -> { // running hold state
                    if ((readInputMask(ic, inputs) & 0x6) == 0) { // if start/reset low
                        writeState(ic, outputs, (byte) 2); // enter running state
                        writeStartTime(ic, outputs, readSysTime(ic, inputs));
                    }
                }

                case 2 -> { // running state
                    if ((readInputMask(ic, inputs) & 0x6) != 0) { // if start/reset high
                        writeState(ic, outputs, (byte) 1); // enter running hold state

                    } else if (readSysTime(ic, inputs) >= readStartTime(ic, inputs) + pointerMax) { // if at max time
                        writeState(ic, outputs, (byte) 3); // enter ticking state
                        writeOutputMask(ic, outputs, (byte) 0x1); // enable pulse output, disable running output
                    }
                }

                case 3 -> { // ticking state
                    if (readSysTime(ic, inputs) >= readStartTime(ic, inputs) + pointerMax + 2) { // if 2 ticks past max time
                        if ((readInputMask(ic, inputs) & 0x4) != 0) {
                            writeState(ic, outputs, (byte) 1); // enter running hold state
                            writeOutputMask(ic, outputs, (byte) 0x8); // enable running output, disable pulse output
                        } else {
                            writeState(ic, outputs, (byte) 0); // enter inactive state
                            writeOutputMask(ic, outputs, (byte) 0); // disable all outputs
                        }
                    }
                }

                default -> {
                    LOGGER.error("Invalid state value: " + stateVal);
                    writeState(ic, outputs, (byte) 0);
                }
            }

        }
    }
}
