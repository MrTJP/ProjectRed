package mrtjp.projectred.fabrication.engine.gates;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.raytracer.IndexedCuboid6;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Vector3;
import mrtjp.fengine.api.IPathFinder;
import mrtjp.fengine.assemble.PathFinderResult;
import mrtjp.projectred.fabrication.engine.*;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.text.ITextProperties;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import static mrtjp.projectred.ProjectRedFabrication.LOGGER;
import static mrtjp.projectred.fabrication.engine.PRFabricationEngine.*;

public class IOGateTile extends RedstoneGateTile implements IIOConnectionTile {

    public static final int COLOUR_PACKET = 6;

    private int regId = REG_ZERO;
    private byte colour = 0;

    public IOGateTile() {
        super(ICTileType.IO_GATE, ICGateTileType.IO.renderIndex);
    }

    @Override
    public void save(CompoundNBT tag) {
        super.save(tag);
        tag.putInt("reg", regId);
        tag.putByte("colour", colour);
    }

    @Override
    public void load(CompoundNBT tag) {
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
            case COLOUR_PACKET:
                colour = in.readByte();
                break;
            default:
                super.read(in, key);
        }
    }

    protected void sendColourUpdate() {
        getWriteStream(COLOUR_PACKET).writeByte(colour);
    }

    @Override
    public boolean isInputIOMode() {
        return getShape() == 0;
    }

    @Override
    public int getIOSide() {
        return getRotation();
    }

    @Override
    public ICConnectionType getConnectionType() {
        return ICConnectionType.BUNDLED;
    }

    protected void toggleWorldInput() {
        LOGGER.info("Toggling world input");
        getEditor().getStateMachine().onInputRegistersChanged(getIOSide(), i -> (short) (i ^ (1<<colour)));
    }

    protected void incrementColour() {
        colour = (byte) ((colour + 1) % 16);
        sendColourUpdate();
        getEditor().markTileChange();
    }

    protected int getStaticOutputRegister(int colour) {
        return outputRegisterId(getIOSide(), colour);
    }

    protected int getStaticInputRegister(int colour) {
        return inputRegisterId(getIOSide(), colour);
    }

    @Override
    public List<IndexedCuboid6> getInteractionZones() {
        List<IndexedCuboid6> zones = new LinkedList<>();

        zones.add(new IndexedCuboid6(0, new Cuboid6(1, 2, 0, 15, 3, 5))); // Toggle state of IO register
        zones.add(new IndexedCuboid6(1, new Cuboid6(2, 2, 5, 6, 3, 8))); // Toggle IO mode
        zones.add(new IndexedCuboid6(2, new Cuboid6(1, 2, 8.5, 5, 4, 12.5)));   // Toggle colour

        Transformation rotation = Rotation.quarterRotations[getRotation()].at(new Vector3(8, 8, 8));
        zones.forEach(c -> c.apply(rotation));

        return zones;
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void buildInteractionToolTip(List<ITextProperties> toolTip, int i) {

        switch (i) { //TODO localize
            case 0:
                toolTip.add(new StringTextComponent("Toggle state"));
                toolTip.add(new StringTextComponent(((getState() & 0x44) != 0 ? "0x1" : "0x0")).withStyle(TextFormatting.GRAY));
                break;
            case 1:
                toolTip.add(new StringTextComponent("Toggle IO mode"));
                toolTip.add(new StringTextComponent((isInputIOMode() ? "input" : "output")).withStyle(TextFormatting.GRAY));
                break;
            case 2:
                toolTip.add(new StringTextComponent("Toggle colour"));
                toolTip.add(new TranslationTextComponent(EnumColour.values()[colour & 0xFF].getUnlocalizedName()).withStyle(TextFormatting.GRAY));
                break;
            default:
        }
    }

    @Override
    public void onInteractionZoneClicked(int i) {

        switch (i) {
            case 0:
                toggleWorldInput();
                break;
            case 1:
                configureAndSend();
                break;
            case 2:
                incrementColour();
                break;
            default:
        }
    }

    // IGateRenderKey overrides

    @Override
    public int state2() {
        return colour & 0xFF;
    }

    // RedstoneGateTile overrides

    @Override
    protected boolean cycleShape() {
        setShape((getShape() + 1) % 2);
        return true;
    }

    @Override
    protected int redstoneOutputMask() {
        return isInputIOMode() ? 0x0 : 0x4;
    }

    @Override
    protected int redstoneInputMask() {
        return isInputIOMode() ? 0x4 : 0x0;
    }

    // BaseTile overrides

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

    // FETile overrides

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
                // TODO log this somewhere
                System.out.println("ERR: Unexpected multiple drivers: " + pfr.outputRegisters);
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
}
