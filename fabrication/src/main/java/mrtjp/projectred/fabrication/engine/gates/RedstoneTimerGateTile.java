package mrtjp.projectred.fabrication.engine.gates;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.vec.Cuboid6;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.editor.tools.InteractionZone;
import mrtjp.projectred.fabrication.editor.tools.SimpleInteractionZone;
import mrtjp.projectred.fabrication.engine.ICSimulationContainer;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;

import java.util.List;
import java.util.function.Consumer;

import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_TIMER_INTERVAL;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_UNIT_ONLY_TICKS;

public abstract class RedstoneTimerGateTile extends TimedStateGateTile {

    public static final int START_TIME_PACKET = 15;
    public static final int MAX_TIME_PACKET = 16;
    protected int pointerMax = 38;
    protected long pointerStartTime = -1;

    public RedstoneTimerGateTile(ICGateTileType gateType) { super(gateType); }

    @Override
    public void save(CompoundTag tag) {
        super.save(tag);
        tag.putLong("tstart", pointerStartTime);
        tag.putInt("tmax", pointerMax);
    }

    @Override
    public void load(CompoundTag tag) {
        super.load(tag);
        pointerStartTime = tag.getLong("tstart");
        pointerMax = tag.getInt("tmax");
    }

    @Override
    public void writeDesc(MCDataOutput out) {
        super.writeDesc(out);
        out.writeLong(pointerStartTime);
        out.writeInt(pointerMax);
    }

    @Override
    public void readDesc(MCDataInput in) {
        super.readDesc(in);
        pointerStartTime = in.readLong();
        pointerMax = in.readInt();
    }

    @Override
    public void read(MCDataInput in, int key) {
        switch (key) {
            case START_TIME_PACKET -> pointerStartTime = in.readLong();
            case MAX_TIME_PACKET -> pointerMax = in.readInt();
            default -> super.read(in, key);
        }
    }

    protected void sendTimeStateUpdate() {
        getWriteStream(START_TIME_PACKET).writeLong(pointerStartTime);
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
        RedstoneTimerGateTile.addTimerAdjustmentInteractionZones(zones, this::addTimerMax);
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

    @Override
    public void onSimRegistersChanged(int rMask, ICSimulationContainer container) {
        super.onSimRegistersChanged(rMask, container);
        long startTime = container.pullLongValue(timeRegs, 0);
        if (startTime != pointerStartTime) {
            pointerStartTime = startTime;
            sendTimeStateUpdate();
        }
    }
    //endregion

    //region IGateRenderData
    @Override
    public abstract boolean isPointerStarted();

    @Override
    public int pointerMax() {
        return pointerMax;
    }

    @Override
    public int pointerValue() {
        long simTime = getEditor().getStateMachine().getSimSystemTime();
        return (int) (simTime - pointerStartTime);
    }
    //endregion

    // Static utility for other gates that need time adjustment zones
    public static void addTimerAdjustmentInteractionZones(List<InteractionZone> zones, Consumer<Integer> adjustFunc) {
        double w = 3 / 16D;
        double h = 0.5 / 16D;
        double l = 1.5 / 16D;
        double x = 0 / 16D;
        double y = 2 / 16D;
        double z = 0 / 16D;

        int[] deltas = new int[] { 200, 20, 1, -1, -20, -200 };
        int split = deltas.length / 2;

        for (int i = 0; i < deltas.length; i++) {
            Cuboid6 box = new Cuboid6(0, 0, 0, w, h, l);

            if (i < split) { // First 'split' buttons on top edge
                box.add(x, y, z + (l * i));
            } else { // Last 'split' buttons on bottom edge
                box.add(x, y, 1 - (l * split) + (l * (i - split)));
            }

            int tDelta = deltas[i];
            double sDelta = tDelta * 0.05;
            Component text = Component.literal("%+.2fs".formatted(sDelta));
            Component toolTip = Component.literal("%+.2fs (%+d ".formatted(sDelta, tDelta))
                    .append(Component.translatable(UL_UNIT_ONLY_TICKS))
                    .append(Component.literal(")"))
                    .withStyle(ICWorkbenchEditor.UNIFORM_GRAY);

            zones.add(new SimpleInteractionZone.Builder()
                    .bounds(() -> box)
                    .boundingBoxLineWidth(2.0)
                    .leftClickAction(() -> adjustFunc.accept(tDelta))
                    .text(text)
                    .tooltip(toolTip)
                    .build());
        }
    }
}
