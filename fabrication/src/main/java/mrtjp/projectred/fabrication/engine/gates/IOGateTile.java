package mrtjp.projectred.fabrication.engine.gates;

import codechicken.lib.colour.EnumColour;
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

import java.util.List;
import java.util.Optional;

import static mrtjp.projectred.fabrication.engine.PRFabricationEngine.*;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.*;

public class IOGateTile extends RedstoneGateTile implements IIOConnectionTile {

    private static final Cuboid6[] INPUT_TOGGLE_ZONE_BOUNDS = new Cuboid6[4];
    private static final Cuboid6[] DIR_ZONE_BOUNDS = new Cuboid6[4];
    private static final Cuboid6[] COLOR_ZONE_BOUNDS = new Cuboid6[4];

    static {
        for (int r = 0; r < 4; r++) {
            Transformation t = new Scale(1/16D).with(Rotation.quarterRotations[r].at(Vector3.CENTER));
            INPUT_TOGGLE_ZONE_BOUNDS[r] = new Cuboid6(1, 2, 0, 15, 3, 5).apply(t);       // Toggle state of IO register
            DIR_ZONE_BOUNDS[r]          = new Cuboid6(2, 2, 5, 6, 3, 8).apply(t);        // Toggle IO mode
            COLOR_ZONE_BOUNDS[r]        = new Cuboid6(1, 2, 8.5, 5, 4, 12.5).apply(t);   // Toggle colour
        }
    }

    public static final int COLOUR_PACKET = 6;

    private int regId = REG_ZERO;
    private byte colour = 0;

    public IOGateTile() {
        super(ICGateTileType.IO);
    }

    @Override
    public void save(CompoundTag tag) {
        super.save(tag);
        tag.putInt("reg", regId);
        tag.putByte("colour", colour);
    }

    @Override
    public void load(CompoundTag tag) {
        super.load(tag);
        regId = tag.getInt("reg");
        colour = tag.getByte("colour");
    }

    @Override
    public void writeDesc(MCDataOutput out) {
        super.writeDesc(out);
        out.writeByte(colour);
    }

    @Override
    public void readDesc(MCDataInput in) {
        super.readDesc(in);
        colour = in.readByte();
    }

    @Override
    public void read(MCDataInput in, int key) {
        switch (key) {
            case COLOUR_PACKET -> colour = in.readByte();
            default -> super.read(in, key);
        }
    }

    protected void sendColourUpdate() {
        getWriteStream(COLOUR_PACKET).writeByte(colour);
    }

    protected void toggleWorldInput() {
        getEditor().getStateMachine().onInputRegistersChanged(getIOSide(), i -> (short) (i ^ (1<<colour)));
    }

    protected void shiftColour(boolean up) {
        colour = (byte) ((colour + (up ? 1 : 15)) % 16);
        sendColourUpdate();
        getEditor().markTileChange();
    }

    protected void toggleDirection() {
        configureShapeAndSend((getShape() + 1) % 2);
    }

    protected int getStaticOutputRegister(int colour) {
        return outputRegisterId(getIOSide(), colour);
    }

    protected int getStaticInputRegister(int colour) {
        return inputRegisterId(getIOSide(), colour);
    }

    //region GateTile overrides
    @Override
    protected boolean canRotate() {
        return false;
    }
    //endregion

    //region IIOConnectionTile overrides
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
    public void buildInteractionZoneList(List<InteractionZone> zones) {
        super.buildInteractionZoneList(zones);

        // For toggling input to simulation
        zones.add(new SimpleInteractionZone.Builder()
                .bounds(() -> INPUT_TOGGLE_ZONE_BOUNDS[getRotation()])
                .leftClickAction(this::toggleWorldInput)
                .tooltip(toolTip -> {
                    toolTip.add(Component.translatable(isInputIOMode() ? UL_SIM_INPUT : UL_SIM_OUTPUT)
                            .append(Component.literal(": " + ((getState() & 0x44) != 0 ? "0x1" : "0x0")))
                            .withStyle(ICWorkbenchEditor.UNIFORM_GRAY));
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

        // For toggling colour
        zones.add(new SimpleInteractionZone.Builder()
                .bounds(() -> COLOR_ZONE_BOUNDS[getRotation()])
                .leftClickAction(() -> shiftColour(true))
                .rightClickAction(() -> shiftColour(false))
                .tooltip(toolTip -> {
                    toolTip.add(Component.translatable(UL_SIGNAL_COLOUR)
                            .append(Component.literal(": "))
                            .append(Component.translatable(EnumColour.values()[colour & 0xFF].getUnlocalizedName()))
                            .withStyle(ICWorkbenchEditor.UNIFORM_GRAY));
                })
                .build());
    }

    @Override
    public void onSimRegistersChanged(int rMask, ICSimulationContainer container) {
        int oldState = getState();
        int newState =  pullInputMask(container) & 0xF | pullOutputMask(container) << 4;
        if (oldState != newState) {
            setState(newState);
            sendStateUpdate();
        }
    }

    protected int pullInputMask(ICSimulationContainer container) {
        return !isInputIOMode() && container.pullRegisterValue(regId) != 0 ? 0x4 : 0;
    }

    protected int pullOutputMask(ICSimulationContainer container) {
        return isInputIOMode() && container.pullRegisterValue(regId) != 0 ? 0x4 : 0;
    }
    //endregion

    //region IGateRenderKey overrides
    @Override
    public int state2() {
        return colour & 0xFF;
    }
    //endregion

    //region RedstoneGateTile overrides
    @Override
    protected int redstoneOutputMask() {
        return isInputIOMode() ? 0x0 : 0x4;
    }

    @Override
    protected int redstoneInputMask() {
        return isInputIOMode() ? 0x4 : 0x0;
    }
    //endregion

    //region FETile overrides
    @Override
    public void allocate(Allocator allocator) {
        if (isInputIOMode()) { // Input from world, output into simulation
            regId = allocator.allocRegisterID(getStaticInputRegister(colour));
        } else { // Input from simulation, output into world
            regId = REG_ZERO; // Will be located, then remapped to target static register
        }
    }

    @Override
    public void locate(IPathFinder pathFinder) {
        if (!isInputIOMode()) {
            int absR = toAbsoluteRotation(2);
            int absDir = IRotatableICTile.rotationToDir(absR);
            PathFinderResult pfr = pathFinder.doPathFinding((d, p) -> d == absDir);
            if (pfr.outputRegisters.size() > 1) {
                getEditor().getStateMachine().getCompilerLog().addProblem(new MultipleDriversError(getPos(), pfr.outputRegisters));
            }
            if (!pfr.outputRegisters.isEmpty()) {
                regId = pfr.outputRegisters.get(0);
            }
        }
    }

    @Override
    public void registerRemaps(RemapRegistry remapRegistry) {
        if (!isInputIOMode() && regId != REG_ZERO) {
            remapRegistry.addRemap(regId, getStaticOutputRegister(colour));
        }
    }

    @Override
    public void consumeRemaps(RemapProvider remapProvider) {
        regId = remapProvider.getRemappedRegisterID(regId);
    }

    @Override
    public void collect(Collector collector) {
        // Static registers are pre-added during assembler instantiation
//        if (isInputIOMode()) {
//            collector.addRegister(regId, new ByteRegister());
//        }
    }

    @Override
    public Optional<Integer> getOutputRegister(int outDir, int outPort) {
        int gateOutputDir = IRotatableICTile.rotationToDir(toAbsoluteRotation(2));
        return isInputIOMode() && outDir == gateOutputDir ? Optional.of(regId) : Optional.empty();
    }

    @Override
    public Optional<Integer> getInputRegister(int inDir, int inPort) {
        int gateInputDir = IRotatableICTile.rotationToDir(toAbsoluteRotation(2));
        return !isInputIOMode() && inDir == gateInputDir ? Optional.of(regId) : Optional.empty();
    }
    //endregion
}
