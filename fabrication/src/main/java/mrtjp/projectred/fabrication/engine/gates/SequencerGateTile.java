package mrtjp.projectred.fabrication.engine.gates;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.fengine.simulate.ICGate;
import mrtjp.fengine.simulate.ICSimulation;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.editor.tools.InteractionZone;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;

import java.util.ArrayList;
import java.util.List;

import static mrtjp.projectred.fabrication.engine.PRFabricationEngine.REG_TIME;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_TIMER_INTERVAL;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_UNIT_ONLY_TICKS;

public class SequencerGateTile extends SidedRedstoneGateTile {

    public static final int MAX_TIME_PACKET = 16;

    private int pointerMax = 38;

    public SequencerGateTile() {
        super(ICGateTileType.SEQUENCER);
    }

    @Override
    public void save(CompoundTag tag) {
        super.save(tag);
        tag.putInt("tmax", pointerMax);
    }

    @Override
    public void load(CompoundTag tag) {
        super.load(tag);
        pointerMax = tag.getInt("tmax");
    }

    @Override
    public void writeDesc(MCDataOutput out) {
        super.writeDesc(out);
        out.writeInt(pointerMax);
    }

    @Override
    public void readDesc(MCDataInput in) {
        super.readDesc(in);
        pointerMax = in.readInt();
    }

    @Override
    public void read(MCDataInput in, int key) {
        switch (key) {
            case MAX_TIME_PACKET -> pointerMax = in.readInt();
            default -> super.read(in, key);
        }
    }

    protected void sendMaxTimeUpdate() {
        getWriteStream(MAX_TIME_PACKET).writeInt(pointerMax);
    }

    //region BaseTile overrides
    @Override
    public void buildToolTip(List<Component> toolTip) {
        super.buildToolTip(toolTip);
        int tmax = pointerMax + 2; // Account for 2-tick high-state
        double smax = tmax * 0.05;
        toolTip.add(Component.translatable(UL_TIMER_INTERVAL)
                .append(Component.literal(": %.2fs (%d ".formatted(smax, tmax)))
                .append(Component.translatable(UL_UNIT_ONLY_TICKS))
                .append(Component.literal(")"))
                .withStyle(ICWorkbenchEditor.UNIFORM_GRAY));
    }

    @Override
    public void buildInteractionZoneList(List<InteractionZone> zones) {
        super.buildInteractionZoneList(zones);
        TimerGateTile.addTimerAdjustmentInteractionZones(zones, this::addTimerMax);
    }

    private void addTimerMax(int delta) {
        int newMax = pointerMax + delta;
        int min = Math.max(4, Configurator.minTimerTicks) - 2;
        if (newMax < min) newMax = min;

        if (newMax != pointerMax) {
            pointerMax = newMax;
            sendMaxTimeUpdate();
            getEditor().markTileChange();
        }
    }
    //endregion

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
        return 0xF;
    }
    //endregion

    //region IGateRenderData
    @Override
    public boolean isPointerStarted() {
        return getEditor().getStateMachine().isSimulating();
    }

    @Override
    public int pointerMax() {
        return pointerMax;
    }

    @Override
    public int pointerValue() {
        long simTime = getEditor().getStateMachine().getSimSystemTime();
        return (int) (simTime % (pointerMax * 4L));
    }
    //endregion


    @Override
    protected void collectGate(Collector collector, int gateId, int[] inputRegisters, int[] outputRegisters) {
        List<Integer> inputRegs = new ArrayList<>();
        List<Integer> outputRegs = new ArrayList<>();

        for (int i = 0; i < 8; i++) {           // 0-8: sysTime
            inputRegs.add(REG_TIME[i]);
        }

        outputRegs.add(outputRegisters[0]);                        // 0: output top
        outputRegs.add(outputRegisters[getShape() == 0 ? 1 : 3]);  // 1: output right
        outputRegs.add(outputRegisters[2]);                        // 2: output bottom
        outputRegs.add(outputRegisters[getShape() == 0 ? 3 : 1]);  // 3: output left

        collector.addGate(gateId, new SequencerGate(pointerMax), inputRegs, outputRegs);
    }

    public static class SequencerGate implements ICGate {

        private final long pointerMax;

        public SequencerGate(long pointerMax) {
            this.pointerMax = pointerMax;
        }

        private static long readSysTime(ICSimulation ic, int[] inputs) { return ic.getRegLongVal(inputs, 0); }

        private static void writeOutputMask(ICSimulation ic, int[] outputs, int mask) {
            ic.queueRegByteVal(outputs[0], (byte) ((mask & 1) != 0 ? 1 : 0));
            ic.queueRegByteVal(outputs[1], (byte) ((mask & 2) != 0 ? 1 : 0));
            ic.queueRegByteVal(outputs[2], (byte) ((mask & 4) != 0 ? 1 : 0));
            ic.queueRegByteVal(outputs[3], (byte) ((mask & 8) != 0 ? 1 : 0));
        }

        @Override
        public void compute(ICSimulation ic, int[] inputs, int[] outputs) {
            long sysTime = readSysTime(ic, inputs);
            int mask = 1 << sysTime % (pointerMax * 4L) / pointerMax;
            writeOutputMask(ic, outputs, mask);
        }
    }
}
