package mrtjp.projectred.fabrication.engine.gates;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.vec.*;
import mrtjp.fengine.api.IPathFinder;
import mrtjp.fengine.assemble.PathFinderResult;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.editor.tools.InteractionZone;
import mrtjp.projectred.fabrication.editor.tools.SimpleInteractionZone;
import mrtjp.projectred.fabrication.engine.ICInterfaceType;
import mrtjp.projectred.fabrication.engine.ICSimulationContainer;
import mrtjp.projectred.fabrication.engine.IIOConnectionTile;
import mrtjp.projectred.fabrication.engine.IRotatableICTile;
import mrtjp.projectred.fabrication.engine.log.MultipleDriversError;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static mrtjp.projectred.fabrication.engine.PRFabricationEngine.*;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.*;

public class BundledBusIOGateTile extends BundledGateTile implements IIOConnectionTile {

    public static final int BUNDLED_SIGNAL_PACKET = 6;

    private static final Cuboid6[] INPUT_TOGGLE_ZONE_BOUNDS = new Cuboid6[4];
    private static final Cuboid6[] DIR_ZONE_BOUNDS = new Cuboid6[4];

    protected final int[] regIds = new int[16];
    protected short bundledSignal = 0;

    {
        Arrays.fill(regIds, -1);
    }

    static {
        for (int r = 0; r < 4; r++) {
            Transformation t = new Scale(1/16D).with(Rotation.quarterRotations[r].at(Vector3.CENTER));
            INPUT_TOGGLE_ZONE_BOUNDS[r] = new Cuboid6(1, 2, 0, 15, 3, 3).apply(t);       // Toggle state of IO register
            DIR_ZONE_BOUNDS[r]          = new Cuboid6(5, 2, 7, 11, 5, 13).apply(t);      // Toggle IO mode
        }
    }

    public BundledBusIOGateTile() {
        super(ICGateTileType.BUNDLED_BUS_IO);
    }

    //region Save/Load
    @Override
    public void save(CompoundTag tag) {
        super.save(tag);
        for (int i = 0; i < 16; i++) {
            tag.putInt("reg" + i, regIds[i]);
        }
        tag.putShort("bundled_signal", bundledSignal);
    }

    @Override
    public void load(CompoundTag tag) {
        super.load(tag);
        for (int i = 0; i < 16; i++) {
            if (tag.contains("reg" + i)) {
                regIds[i] = tag.getInt("reg" + i);
            } else {
                regIds[i] = -1;
            }
        }
        bundledSignal = tag.getShort("bundled_signal");
    }
    //endregion

    //region Network
    @Override
    public void writeDesc(MCDataOutput out) {
        super.writeDesc(out);
        out.writeShort(bundledSignal);
    }

    @Override
    public void readDesc(MCDataInput in) {
        super.readDesc(in);
        bundledSignal = in.readShort();
    }

    @Override
    public void read(MCDataInput in, int key) {
        switch (key) {
            case BUNDLED_SIGNAL_PACKET -> bundledSignal = in.readShort();
            default -> super.read(in, key);
        }
    }

    protected void sendBundledSignalUpdate() {
        getWriteStream(BUNDLED_SIGNAL_PACKET).writeShort(bundledSignal);
    }
    //endregion

    //region IIOConnectionTile implementation
    @Override
    public boolean isInputIOMode() {
        return getShape() == 0;
    }

    @Override
    public int getIOSide() {
        return getRotation();
    }

    @Override
    public ICInterfaceType getInterfaceType() {
        return ICInterfaceType.BUNDLED;
    }
    //endregion

    //region BaseTile overrides
    @Override
    public void onSimRegistersChanged(int rMask, ICSimulationContainer container) {
        int oldSig = bundledSignal;
        int newSig = pullBundledSignal(container);
        if (oldSig != newSig) {
            bundledSignal = (short) newSig;
            sendBundledSignalUpdate();
        }
    }

    protected int pullBundledSignal(ICSimulationContainer container) {
        int signal = 0;
        for (int i = 0; i < 16; i++) {
            if (regIds[i] != -1) {
                signal |= container.pullRegisterValue(regIds[i]) << i;
            }
        }
        return signal;
    }

    @Override
    public void buildInteractionZoneList(List<InteractionZone> zones) {
        super.buildInteractionZoneList(zones);

        // For toggling input to simulation
        zones.add(new SimpleInteractionZone.Builder()
                .bounds(() -> INPUT_TOGGLE_ZONE_BOUNDS[getRotation()])
                .tooltip(toolTip -> {
                    toolTip.add(Component.translatable(isInputIOMode() ? UL_IO_BUNDLED_INPUT : UL_IO_BUNDLED_OUTPUT)
                            .append(Component.literal(": " + "0x%04X".formatted(bundledSignal)))
                            .withStyle(ICWorkbenchEditor.UNIFORM_GRAY));

                    if (isInputIOMode()) {
                        toolTip.add(Component.translatable(UL_IO_BUS_TOGGLE).withStyle(ICWorkbenchEditor.UNIFORM_GRAY.withItalic(true)));
                    }
                })
                .build());

        // For toggling input/output direction
        zones.add(new SimpleInteractionZone.Builder()
                .bounds(() -> DIR_ZONE_BOUNDS[getRotation()])
                .leftClickAction(this::toggleDirection)
                .tooltip(toolTip -> {
                    toolTip.add(Component.translatable(UL_IO_DIRECTION)
                            .append(Component.literal(": "))
                            .append(Component.translatable((isInputIOMode() ? UL_IO_DIR_INPUT : UL_IO_DIR_OUTPUT)))
                            .withStyle(ICWorkbenchEditor.UNIFORM_GRAY));
                })
                .build());

    }

    protected void toggleDirection() {
        configureShapeAndSend((getShape() + 1) % 2);
    }
    //endregion

    //region IGateRenderKey overrides
    @Override
    public short bOutput2() {
        // Renderer should show 0 signal when not simulating
        if (!getEditor().getStateMachine().isSimulating()) return 0;
        return bundledOutputMask() != 0 ? bundledSignal : 0;
    }

    @Override
    public short bInput2() {
        // Renderer should show 0 signal when not simulating
        if (!getEditor().getStateMachine().isSimulating()) return 0;
        return bundledInputMask() != 0 ? bundledSignal : 0;
    }
    //endregion

    //region GateTile overrides
    @Override
    protected boolean canRotate() {
        return false;
    }
    //endregion

    //region BundledGateTile overrides
    @Override
    protected int bundledOutputMask() {
        // Input mode, so this IC gate outputs into the sim
        return isInputIOMode() ? 0x4 : 0x0;
    }

    @Override
    protected int bundledInputMask() {
        // Output mode, so this IC gate inputs into sim
        return isInputIOMode() ? 0x0 : 0x4;
    }
    //endregion

    //region FETile overrides
    @Override
    public void allocate(Allocator allocator) {
        if (isInputIOMode()) { // Input from world, output into simulation
            for (int i = 0; i < 16; i++) {
                regIds[i] = inputRegisterId(getIOSide(), i);
            }
        } else { // Input from simulation, output into world
            Arrays.fill(regIds, REG_ZERO); // Will be located, then remapped to target static register
        }
    }

    @Override
    public void locate(IPathFinder pathFinder) {
        if (!isInputIOMode()) {
            int absR = toAbsoluteRotation(2);
            int absDir = IRotatableICTile.rotationToDir(absR);
            // Run path finding for each of the 16 ports/colors of the bundled connection
            for (int i = 0; i < 16; i++) {
                final int port = i;
                PathFinderResult pfr = pathFinder.doPathFinding((d, p) -> d == absDir && p == port);
                if (pfr.outputRegisters.size() > 1) {
                    //TODO specify port in this error
                    getEditor().getStateMachine().getCompilerLog().addProblem(new MultipleDriversError(getPos(), pfr.outputRegisters));
                }
                if (!pfr.outputRegisters.isEmpty()) {
                    regIds[i] = pfr.outputRegisters.get(0);
                }
            }
        }
    }

    @Override
    public void registerRemaps(RemapRegistry remapRegistry) {
        if (!isInputIOMode()) {
            for (int i = 0; i < 16; i++) {
                if (regIds[i] == REG_ZERO) continue;
                remapRegistry.addRemap(regIds[i], outputRegisterId(getIOSide(), i));
            }
        }
    }

    @Override
    public void consumeRemaps(RemapProvider remapProvider) {
        for (int i = 0; i < 16; i++) {
            regIds[i] = remapProvider.getRemappedRegisterID(regIds[i]);
        }
    }

    @Override
    public void collect(Collector collector) {
        // No registers to define. We are only dealing with static registers that our
        // engine pre-creates as hard-coded inputs/outputs.
    }

    @Override
    public Optional<Integer> getOutputRegister(int outDir, int outPort) {
        int gateOutputDir = IRotatableICTile.rotationToDir(toAbsoluteRotation(2));
        return isInputIOMode() && outDir == gateOutputDir ? Optional.of(regIds[outPort]) : Optional.empty();
    }

    @Override
    public Optional<Integer> getInputRegister(int inDir, int inPort) {
        int gateInputDir = IRotatableICTile.rotationToDir(toAbsoluteRotation(2));
        return !isInputIOMode() && inDir == gateInputDir ? Optional.of(regIds[inPort]) : Optional.empty();
    }
    //endregion
}
