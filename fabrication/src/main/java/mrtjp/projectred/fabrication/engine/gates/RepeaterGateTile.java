package mrtjp.projectred.fabrication.engine.gates;

import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Vector3;
import mrtjp.fengine.simulate.ICGate;
import mrtjp.fengine.simulate.ICSimulation;
import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.LOGGER;
import static mrtjp.projectred.fabrication.engine.PRFabricationEngine.REG_TIME;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_TOGGLE_DELAY;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_UNIT_TICKS;

public class RepeaterGateTile extends TimedStateGateTile {

    private static final int[] DELAYS = { 2, 4, 6, 8, 16, 32, 64, 128, 256 };

    public RepeaterGateTile() {
        super(ICGateTileType.REPEATER);
    }

    //region GateTile overrides

    @Override
    public List<Cuboid6> getInteractionZones() {
        List<Cuboid6> zones = new LinkedList<>();
        zones.add(new Cuboid6(1, 2, 1, 15, 3, 15));
        Transformation rotation = Rotation.quarterRotations[getRotation()].at(new Vector3(8, 8, 8));
        zones.forEach(c -> c.apply(rotation));
        return zones;
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void buildInteractionToolTip(List<Component> toolTip, int i) {

        toolTip.add(Component.translatable(UL_TOGGLE_DELAY));
        toolTip.add(Component.translatable(UL_UNIT_TICKS, DELAYS[getShape()]).withStyle(ChatFormatting.GRAY));
    }

    @Override
    public void onInteractionZoneClicked(int i) {
        configureAndSend();
    }

    //endregion

    //region RedstoneGateTile overrides

    @Override
    protected boolean cycleShape() {
        setShape((getShape() + 1) % DELAYS.length);
        return true;
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
