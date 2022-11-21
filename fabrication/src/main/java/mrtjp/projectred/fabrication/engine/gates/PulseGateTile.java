package mrtjp.projectred.fabrication.engine.gates;

import mrtjp.fengine.simulate.ICGate;
import mrtjp.fengine.simulate.ICSimulation;

import java.util.ArrayList;
import java.util.List;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.LOGGER;
import static mrtjp.projectred.fabrication.engine.PRFabricationEngine.*;

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
        outputRegs.add(outputRegisters[0]);

        collector.addGate(gateId, new PulseGate(), inputRegs, outputRegs);
    }

    public static class PulseGate implements ICGate {

        @Override
        public void compute(ICSimulation ic, int[] inputs, int[] outputs) {

            int stateVal = ic.getRegByteVal(inputs[0]);
            switch (stateVal) {
                case 0: { // Waiting for high input
                    if (ic.getRegByteVal(inputs[5]) != 0) { // if input is high
                        ic.queueRegByteVal(outputs[0], (byte) 1); // go to state 1
                        ic.queueRegByteVal(outputs[5], (byte) 1); // set output high
                        ic.queueRegLongVal(outputs[1], outputs[2], outputs[3], outputs[4],
                                ic.getRegLongVal(inputs[6], inputs[7], inputs[8], inputs[9]) + 2); // set time to t + 2
                    }
                    break;
                }
                case 1: { // Waiting for timer to expire
                    if (ic.getRegLongVal(inputs[6], inputs[7], inputs[8], inputs[9]) >= ic.getRegLongVal(inputs[1], inputs[2], inputs[3], inputs[4])) { // if t is at scheduled time
                        ic.queueRegByteVal(outputs[0], (byte) (ic.getRegByteVal(inputs[5]) == 0 ? 0 : 2)); // if input high, got to state 2, else state 0
                        ic.queueRegByteVal(outputs[5], (byte) 0); // set output low
                        ic.queueRegLongVal(outputs[1], outputs[2], outputs[3], outputs[4], -1); // disable timer
                    }
                    break;
                }
                case 2: { // Waiting for low input state
                    if (ic.getRegByteVal(inputs[5]) == 0) { // if input is low
                        ic.queueRegByteVal(outputs[0], (byte) 0); // go to state 0
                    }
                    break;
                }
                default:
                    LOGGER.error("Invalid state: " + stateVal);
                    ic.queueRegByteVal(outputs[0], (byte) 0); // go to state 0
            }
        }
    }
}
