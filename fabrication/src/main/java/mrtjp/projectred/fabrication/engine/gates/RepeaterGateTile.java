package mrtjp.projectred.fabrication.engine.gates;

import codechicken.lib.vec.*;
import mrtjp.fengine.simulate.ICGate;
import mrtjp.fengine.simulate.ICSimulation;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.editor.tools.InteractionZone;
import mrtjp.projectred.fabrication.editor.tools.SimpleInteractionZone;
import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;

import java.util.ArrayList;
import java.util.List;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.LOGGER;
import static mrtjp.projectred.fabrication.engine.PRFabricationEngine.REG_TIME;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_TIME_DELAY;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_UNIT_TICKS;

public class RepeaterGateTile extends TimedStateGateTile {

    public static final Cuboid6[] DELAY_ZONE_BOUNDS = new Cuboid6[4];

    private static final int[] DELAYS = { 2, 4, 6, 8, 16, 32, 64, 128, 256 };

    static {
        for (int r = 0; r < 4; r++) {
            Transformation t = new Scale(1/16D).with(Rotation.quarterRotations[r].at(Vector3.CENTER));
            DELAY_ZONE_BOUNDS[r] = new Cuboid6(11.5, 2, 3, 13.5, 3, 13).apply(t);
        }
    }

    public RepeaterGateTile() {
        super(ICGateTileType.REPEATER);
    }

    //region GateTile overrides
    @Override
    public void buildInteractionZoneList(List<InteractionZone> zones) {
        super.buildInteractionZoneList(zones);
        zones.add(new SimpleInteractionZone.Builder()
                .bounds(() -> DELAY_ZONE_BOUNDS[getRotation()])
                .leftClickAction(() -> shiftDelay(true))
                .rightClickAction(() -> shiftDelay(false))
                .tooltip(toolTip -> {
                    toolTip.add(Component.translatable(UL_TIME_DELAY)
                            .append(Component.literal(": "))
                            .append(Component.translatable(UL_UNIT_TICKS, DELAYS[getShape()]).withStyle(ChatFormatting.GRAY))
                            .withStyle(ICWorkbenchEditor.UNIFORM_GRAY));
                })
                .build());
    }
    //endregion

    protected void shiftDelay(boolean up) {
        configureShapeAndSend((getShape() + (up ? 1 : DELAYS.length - 1)) % DELAYS.length);
    }

    //region RedstoneGateTile overrides

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

        collector.addGate(gateId, new RepeaterGate(DELAYS[getShape()]), inputRegs, outputRegs);
    }

    public static class RepeaterGate implements ICGate {

        private final int delay;

        public RepeaterGate(int delay) {
            this.delay = delay;
        }

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
                        writeSchedTime(ic, outputs, readSysTime(ic, inputs) + delay); // set timer to t + delay
                    }
                }
                case 1 -> { // Waiting for timer to go high
                    if (readSysTime(ic, inputs) >= readSchedTime(ic, inputs)) { // if timer expired
                        writeState(ic, outputs, (byte) 2); // go to state 2
                        writeOutput(ic, outputs, (byte) 1); // set output high
                        writeSchedTime(ic, outputs, -1); // disable timer
                    }
                }
                case 2 -> { // Waiting for low input
                    if (readInput(ic, inputs) == 0) { // if input is low
                        writeState(ic, outputs, (byte) 3); // go to state 3
                        writeSchedTime(ic, outputs, readSysTime(ic, inputs) + delay); // set timer to t + delay
                    }
                }
                case 3 -> { // Waiting for timer to go low
                    if (readSchedTime(ic, inputs) >= readSchedTime(ic, inputs)) { // if timer expired
                        writeState(ic, outputs, (byte) 0); // go to state 0
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
