package mrtjp.projectred.fabrication.engine.gates;

import codechicken.lib.vec.*;
import mrtjp.fengine.simulate.ICGate;
import mrtjp.fengine.simulate.ICSimulation;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.editor.tools.InteractionZone;
import mrtjp.projectred.fabrication.editor.tools.SimpleInteractionZone;
import net.minecraft.network.chat.Component;

import java.util.ArrayList;
import java.util.List;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.LOGGER;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.*;

public class ToggleLatchGateTile extends InternalStateGateTile {

    private static final Cuboid6[] LEVER_ZONE_BOUNDS = new Cuboid6[4];

    static {
        for (int r = 0; r < 4; r++) {
            Transformation t = new Scale(1/16D).with(Rotation.quarterRotations[r].at(Vector3.CENTER));
            LEVER_ZONE_BOUNDS[r] = new Cuboid6(9, 2, 4, 13, 4, 12).apply(t);
        }
    }

    public ToggleLatchGateTile() {
        super(ICGateTileType.TOGGLE_LATCH);
    }

    //region GateTile overrides
    @Override
    public void buildInteractionZoneList(List<InteractionZone> zones) {
        super.buildInteractionZoneList(zones);
        zones.add(new SimpleInteractionZone.Builder()
                .bounds(() -> LEVER_ZONE_BOUNDS[getRotation()])
                .leftClickAction(this::toggleDefaultState)
                .tooltip(toolTip -> {
                    toolTip.add(Component.translatable(UL_DEFAULT_STATE)
                            .append(Component.literal(": "))
                            .append(Component.translatable(getShape() == 0 ? UL_TOP : UL_BOTTOM))
                            .withStyle(ICWorkbenchEditor.UNIFORM_GRAY));
                })
                .build());
    }
    //endregion

    protected void toggleDefaultState() {
        configureShapeAndSend((getShape() + 1) % 2);
    }

    //region RedstoneGateTile overrides
    @Override
    protected int redstoneInputMask() {
        return 0xA;
    }

    @Override
    protected int redstoneOutputMask() {
        return 5;
    }
    //endregion

    //region IGateRenderData
    @Override
    public int state() {
        if (getEditor().getStateMachine().isSimulating()) {
            return super.state();
        }
        // Force state to make render reflect lever pos outside of simulation.
        // Required since renderer uses output state to determine pos, and that state is
        // not valid until simulation starts.
        return getShape() == 0 ? 0x10 : 0x40;
    }
    //endregion

    @Override
    protected void collectGate(Collector collector, int gateId, int[] inputRegisters, int[] outputRegisters) {

        List<Integer> inputRegs = new ArrayList<>();
        List<Integer> outputRegs = new ArrayList<>();

        inputRegs.add(stateReg);
        inputRegs.add(inputRegisters[3]); //inA
        inputRegs.add(inputRegisters[1]); //inB

        outputRegs.add(stateReg);
        outputRegs.add(outputRegisters[0]); //outA
        outputRegs.add(outputRegisters[2]); //outB

        collector.addGate(gateId, new ToggleLatchGate(getShape()), inputRegs, outputRegs);
    }

    public static class ToggleLatchGate implements ICGate {

        private final int defaultState;

        public ToggleLatchGate(int defaultState) {
            this.defaultState = defaultState;
        }

        private static byte readState(ICSimulation ic, int[] inputs) { return ic.getRegByteVal(inputs[0]); }

        private static int readInputMask(ICSimulation ic, int[] inputs) {
            int mask = 0;
            if (ic.getRegByteVal(inputs[1]) != 0) mask |= 1; //inA
            if (ic.getRegByteVal(inputs[2]) != 0) mask |= 2; //inB
            return mask;
        }

        private static void writeState(ICSimulation ic, int[] outputs, byte state) { ic.queueRegByteVal(outputs[0], state); }

        private static void writeOutputMask(ICSimulation ic, int[] outputs, int mask) {
            ic.queueRegByteVal(outputs[1], (byte) ((mask & 1) != 0 ? 1 : 0)); // outA
            ic.queueRegByteVal(outputs[2], (byte) ((mask & 2) != 0 ? 1 : 0)); // outB
        }

        @Override
        public void compute(ICSimulation ic, int[] inputs, int[] outputs) {
            int s = readState(ic, inputs);
            int state = s & 0xF;
            int inputMask = readInputMask(ic, inputs);
            int prevInputMask = (s >> 4) & 0xF;
            int highMask = inputMask & ~prevInputMask;
            int nextState = state;

            switch (state) {
                case 0: { // Initial state
                    if (defaultState == 0) {
                        nextState = 1; // Enter A state
                        writeOutputMask(ic, outputs, 0x1);
                    } else {
                        nextState = 2; // Enter B state
                        writeOutputMask(ic, outputs, 0x2);
                    }
                    break;
                }

                case 1: { // A state
                    if (highMask == 1 || highMask == 2) {
                        nextState = 2; // Enter B state
                        writeOutputMask(ic, outputs, 0x2);
                    }
                    break;
                }

                case 2: { // B state
                    if (highMask == 1 || highMask == 2) {
                        nextState = 1; // Enter A state
                        writeOutputMask(ic, outputs, 0x1);
                    }
                    break;
                }

                default:
                    LOGGER.error("Invalid state: " + readState(ic, inputs));
                    nextState = 0;
            }

            writeState(ic, outputs, (byte) (inputMask << 4 | nextState));
        }
    }
}
