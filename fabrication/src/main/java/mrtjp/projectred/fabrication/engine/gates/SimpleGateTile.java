package mrtjp.projectred.fabrication.engine.gates;

import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Scale;
import codechicken.lib.vec.Vector3;
import mrtjp.fengine.simulate.ICGate;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.editor.tools.InteractionZone;
import mrtjp.projectred.fabrication.editor.tools.SimpleInteractionZone;
import net.minecraft.network.chat.Component;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_SIDE_DISABLED;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_SIDE_ENABLED;

public abstract class SimpleGateTile extends SidedRedstoneGateTile {

    public static final Cuboid6[] ZONE_BOUNDS = new Cuboid6[16];

    static {
        Cuboid6[] sideBoxes = {
            new Cuboid6(6, 2, 0, 10, 2.5, 6),
            new Cuboid6(10, 2, 6, 16, 2.5, 10),
            new Cuboid6(6, 2, 10, 10, 2.5, 16),
            new Cuboid6(0, 2, 6, 6, 2.5, 10),
        };

        for (int sr = 0; sr < 4; sr++) {
            for (int tr = 0; tr < 4; tr++) {
                int i = sr << 2 | tr;
                ZONE_BOUNDS[i] = sideBoxes[sr].copy()
                        .apply(new Scale(1/16D))
                        .apply(Rotation.quarterRotations[tr].at(Vector3.CENTER));
            }
        }
    }

    public SimpleGateTile(ICGateTileType gateType) {
        super(gateType);
    }

    //region BaseTile overrides
    @Override
    public void buildInteractionZoneList(List<InteractionZone> zones) {
        super.buildInteractionZoneList(zones);
        addDeadSidesInteractions(zones, interactMask(), this::getBoundsForIOToggleZone, this::toggleDeadSide, this::getDeadSideToolTip);
    }

    @OnlyIn(Dist.CLIENT)
    protected Component getDeadSideToolTip(int r) {
        boolean isEnabled = (getShape() & rotationToDeadSideBit(r)) == 0;
        return Component.translatable(isEnabled ? UL_SIDE_ENABLED : UL_SIDE_DISABLED).withStyle(ICWorkbenchEditor.UNIFORM_GRAY);
    }

    protected Cuboid6 getBoundsForIOToggleZone(int r) {
        return ZONE_BOUNDS[getRotation() << 2 | r];
    }

    protected void toggleDeadSide(int r) {
        if (getDeadSides() == 0) return;

        int oldShape = getShape();
        int shape = oldShape ^ rotationToDeadSideBit(r);
        if (Integer.bitCount(shape) > getMaxDeadSides()) return;

        configureShapeAndSend(shape);
    }

    public static void addDeadSidesInteractions(List<InteractionZone> zones, int mask, Function<Integer, Cuboid6> boundsForR, Consumer<Integer> toggleForR, Function<Integer, Component> tooltipForR) {
        for (int r = 0; r < 4; r++) {
            if ((mask & (1 << r)) == 0) continue;
            final int fr = r;
            zones.add(new SimpleInteractionZone.Builder()
                    .bounds(() -> boundsForR.apply(fr))
                    .leftClickAction(() -> toggleForR.accept(fr))
                    .tooltip(() -> tooltipForR.apply(fr))
                    .build());
        }
    }
    //endregion

    //region SimpleGateTile logic override points
    protected int interactMask() {
        return 0;
    }

    protected int getDeadSides() {
        return 0;
    }

    protected int getMaxDeadSides() {
        return getDeadSides() - 1;
    }

    protected int rotationToDeadSideBit(int r) {
        return 1 << (r - 1);
    }

    protected abstract ICGate createGate();

    protected void collectGate(Collector collector, int gateId, int[] inputRegisters, int[] outputRegisters) {

        List<Integer> inputRegistersList = new ArrayList<>();
        List<Integer> outputRegistersList = new ArrayList<>();
        for (int r = 0; r < 4; r++) {
            if (inputRegisters[r] != -1) inputRegistersList.add(inputRegisters[r]);
            if (outputRegisters[r] != -1) outputRegistersList.add(outputRegisters[r]);
        }

        collector.addGate(gateId, createGate(), inputRegistersList, outputRegistersList);
    }
    //endregion
}
