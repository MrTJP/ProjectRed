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
import static mrtjp.projectred.fabrication.engine.PRFabricationEngine.*;
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

        collector.addGate(gateId, new RepeaterGate(DELAYS[getShape()]), inputRegs, outputRegs);
    }

    public static class RepeaterGate implements ICGate {

        private final int delay;

        public RepeaterGate(int delay) {
            this.delay = delay;
        }

        @Override
        public void compute(ICSimulation ic, int[] inputs, int[] outputs) {

            int stateVal = ic.getRegByteVal(inputs[0]);
            switch (stateVal) {
                case 0: { // Waiting for high input
                    if (ic.getRegByteVal(inputs[5]) != 0) { // if input is high
                        ic.queueRegByteVal(outputs[0], (byte) 1); // go to state 1
                        ic.queueRegLongVal(outputs[1], outputs[2], outputs[3], outputs[4],
                                ic.getRegLongVal(inputs[6], inputs[7], inputs[8], inputs[9]) + delay); // set timer to t + delay
                    }
                    break;
                }
                case 1: { // Waiting for timer to go high
                    if (ic.getRegLongVal(inputs[6], inputs[7], inputs[8], inputs[9]) >= ic.getRegLongVal(inputs[1], inputs[2], inputs[3], inputs[4])) { // if timer expired
                        ic.queueRegByteVal(outputs[0], (byte) 2); // go to state 2
                        ic.queueRegByteVal(outputs[5], (byte) 1); // set output high
                        ic.queueRegLongVal(outputs[1], outputs[2], outputs[3], outputs[4], -1); // disable timer
                    }
                    break;
                }
                case 2: { // Waiting for low input
                    if (ic.getRegByteVal(inputs[5]) == 0) { // if input is low
                        ic.queueRegByteVal(outputs[0], (byte) 3); // go to state 3
                        ic.queueRegLongVal(outputs[1], outputs[2], outputs[3], outputs[4],
                                ic.getRegLongVal(inputs[6], inputs[7], inputs[8], inputs[9]) + delay); // set timer to t + delay
                    }
                    break;
                }
                case 3: { // Waiting for timer to go low
                    if (ic.getRegLongVal(inputs[6], inputs[7], inputs[8], inputs[9]) >= ic.getRegLongVal(inputs[1], inputs[2], inputs[3], inputs[4])) { // if timer expired
                        ic.queueRegByteVal(outputs[0], (byte) 0); // go to state 0
                        ic.queueRegByteVal(outputs[5], (byte) 0); // set output low
                        ic.queueRegLongVal(outputs[1], outputs[2], outputs[3], outputs[4], -1); // disable timer
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
