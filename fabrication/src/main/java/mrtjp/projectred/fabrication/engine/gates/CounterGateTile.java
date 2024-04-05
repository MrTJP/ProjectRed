package mrtjp.projectred.fabrication.engine.gates;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.vec.Cuboid6;
import mrtjp.fengine.simulate.ByteRegister;
import mrtjp.fengine.simulate.ICGate;
import mrtjp.fengine.simulate.ICSimulation;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.editor.tools.InteractionZone;
import mrtjp.projectred.fabrication.editor.tools.SimpleInteractionZone;
import mrtjp.projectred.fabrication.engine.ICSimulationContainer;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.LOGGER;
import static mrtjp.projectred.fabrication.engine.IRotatableICTile.flipMaskZ;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.*;

public class CounterGateTile extends InternalStateGateTile {

    private static final int KEY_VALUE = 10;
    private static final int KEY_MAX = 11;
    private static final int KEY_INCR = 12;
    private static final int KEY_DECR = 13;

    // Count stored as short
    private int[] countRegs = new int[] { -1, -1 };

    private int value = 0;
    private int max = 10;
    private int incr = 1;
    private int decr = 1;

    public CounterGateTile() {
        super(ICGateTileType.COUNTER);
    }

    //region save/load
    @Override
    public void save(CompoundTag tag) {
        super.save(tag);
        tag.putIntArray("reg_count", countRegs);
        tag.putShort("val", (short) value);
        tag.putShort("max", (short) max);
        tag.putShort("inc", (short) incr);
        tag.putShort("dec", (short) decr);
    }

    @Override
    public void load(CompoundTag tag) {
        super.load(tag);
        countRegs = tag.getIntArray("reg_count");
        value = tag.getShort("val") & 0xFFFF;
        max = tag.getShort("max") & 0xFFFF;
        incr = tag.getShort("inc") & 0xFFFF;
        decr = tag.getShort("dec") & 0xFFFF;
    }

    @Override
    public void writeDesc(MCDataOutput packet) {
        super.writeDesc(packet);
        packet.writeShort(value);
        packet.writeShort(max);
        packet.writeShort(incr);
        packet.writeShort(decr);
    }

    @Override
    public void readDesc(MCDataInput packet) {
        super.readDesc(packet);
        value = packet.readUShort();
        max = packet.readUShort();
        incr = packet.readUShort();
        decr = packet.readUShort();
    }
    //endregion

    //region Packets
    @Override
    public void read(MCDataInput packet, int key) {
        switch (key) {
            case KEY_VALUE -> value = packet.readUShort();
            case KEY_MAX -> max = packet.readUShort();
            case KEY_INCR -> incr = packet.readUShort();
            case KEY_DECR -> decr = packet.readUShort();
            default -> super.read(packet, key);
        }
    }

    protected void sendValueUpdate() {
        getWriteStream(KEY_VALUE).writeShort(value);
    }

    protected void sendMaxUpdate() {
        getWriteStream(KEY_MAX).writeShort(max);
    }

    protected void sendIncrUpdate() {
        getWriteStream(KEY_INCR).writeShort(incr);
    }

    protected void sendDecrUpdate() {
        getWriteStream(KEY_DECR).writeShort(decr);
    }
    //endregion

    //region BaseTile overrides
    @Override
    public void onSimRegistersChanged(int rMask, ICSimulationContainer container) {
        super.onSimRegistersChanged(rMask, container);
        int value = container.pullShortValue(countRegs, 0) & 0xFFFF;
        if (value != this.value) {
            this.value = value;
            sendValueUpdate();
        }
    }

    @Override
    public void buildToolTip(List<Component> toolTip) {
        super.buildToolTip(toolTip);

        toolTip.add(Component.translatable(UL_COUNTER_VALUE)
                .append(Component.literal(": " + value))
                .withStyle(ICWorkbenchEditor.UNIFORM_GRAY));
        toolTip.add(Component.translatable(UL_COUNTER_MAX)
                .append(Component.literal(": " + max))
                .withStyle(ICWorkbenchEditor.UNIFORM_GRAY));
        toolTip.add(Component.translatable(UL_COUNTER_INCR)
                .append(Component.literal(": " + incr))
                .withStyle(ICWorkbenchEditor.UNIFORM_GRAY));
        toolTip.add(Component.translatable(UL_COUNTER_DECR)
                .append(Component.literal(": " + decr))
                .withStyle(ICWorkbenchEditor.UNIFORM_GRAY));
    }

    @Override
    public void buildInteractionZoneList(List<InteractionZone> zones) {
        super.buildInteractionZoneList(zones);

        double w = 2/16D;
        double h = 0.5/16D;
        double l = 1/16D;

        // max adjust
        double x = 0 / 16D;
        double y = 2 / 16D;
        double z = 0 / 16D;

        int[] deltas = new int[] { -10, -5, -1, 1, 5, 10 };
        String[] names = new String[] { UL_COUNTER_MAX, UL_COUNTER_INCR, UL_COUNTER_DECR };
        List<Consumer<Integer>> actions = List.of(this::addMax, this::addIncr, this::addDecr);

        for (int j = 0; j < 3; j++) {
            for (int i = 0; i < 6; i++) {
                final Consumer<Integer> action = actions.get(j);
                final int delta = deltas[i];

                Component text = Component.literal("%+d".formatted(deltas[i]));
                Component toolTip = Component.translatable(names[j])
                        .append(Component.literal(" %+d".formatted(deltas[i])))
                        .withStyle(ICWorkbenchEditor.UNIFORM_GRAY);

                Cuboid6 box = new Cuboid6(0, 0, 0, w, h, l).add(x + i < 3 ? (i * w) : 1 - (3 * w) + ((i - 3) * w), y, z + (j * l));
                zones.add(new SimpleInteractionZone.Builder()
                        .bounds(box)
                        .boundingBoxLineWidth(2.0)
                        .leftClickAction(() -> action.accept(delta))
                        .text(text)
                        .tooltip(toolTip)
                        .build());
            }
        }

    }

    private void addMax(int delta) {
        int oldMax = max;
        max = Math.max(1, Math.min(max + delta, Short.MAX_VALUE)); // Technically, this can be 2x since we store unsigned
        if (max != oldMax) {
            sendMaxUpdate();
            if (incr > max) {
                incr = max;
                sendIncrUpdate();
            }
            if (decr > max) {
                decr = max;
                sendDecrUpdate();
            }
            getEditor().markTileChange();
        }
    }

    private void addIncr(int delta) {
        int oldIncr = incr;
        incr = Math.max(1, Math.min(incr + delta, max));
        if (incr != oldIncr) {
            sendIncrUpdate();
            getEditor().markTileChange();
        }
    }

    private void addDecr(int delta) {
        int oldDecr = decr;
        decr = Math.max(1, Math.min(decr + delta, max));
        if (decr != oldDecr) {
            sendDecrUpdate();
            getEditor().markTileChange();
        }
    }
    //endregion

    //region GateTile overrides
    @Override
    protected void reflectAndSend() {
        configureShapeAndSend(getShape() == 0 ? 1 : 0);
    }

    @Override
    protected boolean canReflect() {
        return true;
    }
    //endregion

    //region Gate render data
    @Override
    public boolean isPointerStarted() {
        return getEditor().getStateMachine().isSimulating();
    }

    @Override
    public int pointerValue() {
        return value;
    }

    @Override
    public int pointerMax() {
        return max;
    }

    @Override
    public int state() {
        // required bc renderer expects state to be stored reflected
        int state = super.state();
        return getShape() == 0 ? state : state & 0xF0 | flipMaskZ(state) & 0xF;
    }
    //endregion

    //region RedstoneGateTile overrides
    @Override
    protected int redstoneOutputMask() {
        return 0x5;
    }

    @Override
    protected int redstoneInputMask() {
        return 0xA;
    }
    //endregion

    @Override
    protected void clearRegisterIds() {
        super.clearRegisterIds();
        Arrays.fill(countRegs, -1);
    }

    //region FETile overrides
    @Override
    public void allocate(Allocator allocator) {
        super.allocate(allocator);
        for (int i = 0; i < 2; i++) {
            countRegs[i] = allocator.allocRegisterID();
        }
    }

    @Override
    public void consumeRemaps(RemapProvider remapProvider) {
        super.consumeRemaps(remapProvider);
        for (int i = 0; i < 2; i++) {
            countRegs[i] = remapProvider.getRemappedRegisterID(countRegs[i]);
        }
    }

    @Override
    public void collect(Collector collector) {
        super.collect(collector);
        for (int i = 0; i < 2; i++) {
            collector.addRegister(countRegs[i], new ByteRegister());
        }
    }

    @Override
    protected void collectGate(Collector collector, int gateId, int[] inputRegisters, int[] outputRegisters) {
        List<Integer> inputRegs = new ArrayList<>();
        List<Integer> outputRegs = new ArrayList<>();

        inputRegs.add(stateReg);                                 // 0: state
        inputRegs.add(inputRegisters[getShape() == 0 ? 1 : 3]);  // 1: incr input
        inputRegs.add(inputRegisters[getShape() == 0 ? 3 : 1]);  // 2: decr input
        for (int i = 0; i < 2; i++) {                            // 3-4: counter value
            inputRegs.add(countRegs[i]);
        }

        outputRegs.add(stateReg);                                // 0: state
        outputRegs.add(outputRegisters[0]);                      // 1: max output
        outputRegs.add(outputRegisters[2]);                      // 2: min output
        for (int i = 0; i < 2; i++) {                            // 3-4: counter value
            outputRegs.add(countRegs[i]);
        }

        collector.addGate(gateId, new CounterGate(max, incr, decr), inputRegs, outputRegs);
    }

    public static class CounterGate implements ICGate {

        private final int max;
        private final int incr;
        private final int decr;

        public CounterGate(int max, int incr, int decr) {
            this.max = max;
            this.incr = incr;
            this.decr = decr;
        }

        private static byte readState(ICSimulation ic, int[] inputs) { return ic.getRegByteVal(inputs[0]); }
        private static byte readInputMask(ICSimulation ic, int[] inputs) {
            int mask = 0;
            if (ic.getRegByteVal(inputs[1]) != 0) mask |= 2; // incr
            if (ic.getRegByteVal(inputs[2]) != 0) mask |= 8; // decr
            return (byte) mask;
        }
        private static short readCount(ICSimulation ic, int[] inputs) { return ic.getRegShortVal(inputs, 3); }
        private static void writeState(ICSimulation ic, int[] outputs, byte state) { ic.queueRegByteVal(outputs[0], state); }
        private static void writeOutputMask(ICSimulation ic, int[] outputs, byte oMask) {
            ic.queueRegByteVal(outputs[1], (byte) ((oMask & 2) != 0 ? 1 : 0)); // max
            ic.queueRegByteVal(outputs[2], (byte) ((oMask & 4) != 0 ? 1 : 0)); // min
        }
        private static void writeCount(ICSimulation ic, int[] outputs, short count) { ic.queueRegShortVal(outputs, 3, count); }

        @Override
        public void compute(ICSimulation ic, int[] inputs, int[] outputs) {
            int stateVal = readState(ic, inputs);
            int state = stateVal & 0xF;
            int prevInputMask = (stateVal >> 4) & 0xF;
            int inputMask = readInputMask(ic, inputs);
            int highMask = inputMask & ~prevInputMask;

            switch (state) {
                case 0 -> { // Initial state
                    writeState(ic, outputs, (byte) 1);          // min high state
                    writeOutputMask(ic, outputs, (byte) 0x4);   // min output high
                    writeCount(ic, outputs, (short) 0);         // reset count
                }

                case 1 -> { // Running state
                    if (highMask == 0x2) { // If only incr went high
                        int newCount = Math.min(max, (readCount(ic, inputs) & 0xFFFF) + incr);  // increment
                        writeCount(ic, outputs, (short) newCount);                              // update count
                        writeOutputMask(ic, outputs, (byte) (newCount == max ? 0x2 : 0x0));     // write output

                    } else if (highMask == 0x8) { // If only decr went high
                        int newCount = (Math.max(0, (readCount(ic, inputs) & 0xFFFF) - decr));  // decrement
                        writeCount(ic, outputs, (short) newCount);                              // update count
                        writeOutputMask(ic, outputs, (byte) (newCount == 0 ? 0x4 : 0x0));       // write output
                    }

                    writeState(ic, outputs, (byte) (inputMask << 4 | 1)); // stay in running state
                }
                default -> {
                    LOGGER.error("Invalid state: " + state);
                    writeState(ic, outputs, (byte) 0); // reset to initial state
                }
            }
        }
    }
    //endregion
}