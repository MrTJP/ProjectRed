package mrtjp.projectred.fabrication.engine.gates;

import codechicken.lib.raytracer.IndexedCuboid6;
import codechicken.lib.vec.*;
import mrtjp.fengine.simulate.ICGate;
import mrtjp.projectred.fabrication.engine.ICTileType;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.text.ITextProperties;
import net.minecraft.util.text.StringTextComponent;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

public abstract class SimpleGateTile extends SidedRedstoneGateTile {

    public static int[] DEAD_SIDES_MASKS = new int[] { 1, 2, 3, 0, 5, 6, 3 };

    public SimpleGateTile(ICTileType tileType, int renderIndex) {
        super(tileType, renderIndex);
    }

    @Override
    public void save(CompoundNBT tag) {
        super.save(tag);
    }

    @Override
    public void load(CompoundNBT tag) {
        super.load(tag);
    }

    @Override
    protected boolean cycleShape() {
        int oldShape = getShape();
        setShape(progressDeadSideShape(oldShape));
        return oldShape != getShape();
    }

    private int progressDeadSideShape(int shape) {
        if (getDeadSides() == 0) return shape;

        int s = DEAD_SIDES_MASKS[shape];
        s = ensureMaxDeadSides(s);
        return s;
    }

    private int ensureMaxDeadSides(int s) {
        while (Integer.bitCount(s) > getMaxDeadSides() || 32 - Integer.numberOfLeadingZeros(s) > getDeadSides()) {
            s = DEAD_SIDES_MASKS[s];
        }
        return s;
    }

    //region BaseTile overrides

    @Override
    public List<IndexedCuboid6> getInteractionZones() {
        List<IndexedCuboid6> zones = new LinkedList<>();
        if (getDeadSides() > 0) {
            zones.add(new IndexedCuboid6(1, new Cuboid6(10, 2, 6, 16, 2.5, 10)));
            zones.add(new IndexedCuboid6(2, new Cuboid6(6, 2, 10, 10, 2.5, 16)));
            zones.add(new IndexedCuboid6(3, new Cuboid6(0, 2, 6, 6, 2.5, 10)));

            Transformation rotation = Rotation.quarterRotations[getRotation()].at(new Vector3(8, 8, 8));
            Transformation t = isReflected() ? new Scale(1, -1, 1).with(rotation) : rotation;
            zones.forEach(c -> c.apply(t));
        }

        return zones;
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void buildInteractionToolTip(List<ITextProperties> toolTip, int i) {

        boolean isEnabled = (getShape() & (1 << (i-1))) == 0;
        toolTip.add(new StringTextComponent("side " + (isEnabled ? "enabled" : "disabled"))); // TODO localize
    }

    @Override
    public void onInteractionZoneClicked(int i) {
        if (getDeadSides() == 0) return;

        int oldShape = getShape();
        int shape = oldShape ^ (1 << (i-1));
        shape = ensureMaxDeadSides(shape);

        if (oldShape != shape) {
            setShape(shape);
            sendShapeUpdate();
            notifyNeighbors(0xF);
            getEditor().markTileChange();
        }
    }
    //endregion

    //region SimpleGateTile logic override points

    protected int getDeadSides() {
        return 0;
    }

    protected int getMaxDeadSides() {
        return getDeadSides() - 1;
    }

    protected boolean isReflected() {
        return false;
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
