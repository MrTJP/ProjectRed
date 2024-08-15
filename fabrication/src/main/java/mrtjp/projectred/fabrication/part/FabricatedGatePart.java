package mrtjp.projectred.fabrication.part;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.multipart.util.MultipartPlaceContext;
import mrtjp.fengine.api.ICFlatMap;
import mrtjp.projectred.fabrication.editor.EditorDataUtils;
import mrtjp.projectred.fabrication.engine.ICSimulationContainer;
import mrtjp.projectred.fabrication.engine.InterfaceSpec;
import mrtjp.projectred.fabrication.engine.PRFabricationEngine;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.part.BundledGatePart;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.item.ItemStack;

import javax.annotation.Nullable;
import java.util.Objects;
import java.util.function.Function;

import static mrtjp.projectred.core.BundledSignalsLib.*;
import static mrtjp.projectred.fabrication.ProjectRedFabrication.LOGGER;
import static mrtjp.projectred.fabrication.editor.EditorDataUtils.*;

public class FabricatedGatePart extends BundledGatePart {

    private final ICSimulationContainer simulationContainer = new ICSimulationContainer();
    private final InterfaceSpec ifSpec = new InterfaceSpec();

    private CompoundTag itemStackTag = new CompoundTag();
    private String icName = "untitled";
    private long simulationTimeStart = -1L;
    private int compileFormat = 0;

    public FabricatedGatePart() {
        super(GateType.FABRICATED_GATE);
    }

    @Override
    public boolean preparePlacement(MultipartPlaceContext context) {

        if (!super.preparePlacement(context)) return false;

        if (context.getPlayer() == null || context.getPlayer().level().isClientSide()) return false;

        ItemStack stack = context.getItemInHand();
        if (stack.isEmpty() || !stack.hasTag()) {
            LOGGER.warn("Gate placement issue: no NBT on gate item");
            return false;
        }

        CompoundTag tag = stack.getTag();
        if (!EditorDataUtils.canFabricate(tag)) {
            LOGGER.warn("Gate placement issue: gate item contains invalid data");
            return false;
        }

        itemStackTag = EditorDataUtils.createFabricationCopy(tag);
        icName = tag.getString(KEY_IC_NAME);
        ICFlatMap flatMap = PRFabricationEngine.instance.deserializeFlatMap(tag.getString(KEY_FLAT_MAP));
        simulationContainer.setFlatMap(flatMap);
        ifSpec.loadFrom(tag, KEY_IO_SPEC);
        compileFormat = tag.getInt(KEY_COMPILE_FORMAT);

        return true;
    }

    @Override
    public void save(CompoundTag tag) {
        super.save(tag);
        tag.put("item_stack", itemStackTag);
        tag.putString("ic_name", icName);
        tag.putLong("sim_time", level().getGameTime() - simulationTimeStart);
        tag.putInt("compile_format", compileFormat);
        simulationContainer.save(tag);
        ifSpec.saveTo(tag, "io_spec");
    }

    @Override
    public void load(CompoundTag tag) {
        super.load(tag);
        itemStackTag = tag.getCompound("item_stack");
        icName = tag.getString("ic_name");
        simulationTimeStart = tag.getLong("sim_time");
        compileFormat = tag.getInt("compile_format");
        if (compileFormat == PRFabricationEngine.COMPILE_FORMAT) {
            simulationContainer.load(tag);
        } else {
            LOGGER.warn("Fabricated Gate compile format mismatch ({} vs {})", compileFormat, PRFabricationEngine.COMPILE_FORMAT);
        }
        ifSpec.loadFrom(tag, "io_spec");
    }

    @Override
    public void writeDesc(MCDataOutput packet) {
        super.writeDesc(packet);
        packet.writeCompoundNBT(itemStackTag); // Client needs tag for pick-blcok
        packet.writeString(icName);
        ifSpec.writeDesc(packet);
        packet.writeInt(compileFormat);
    }

    @Override
    public void readDesc(MCDataInput packet) {
        super.readDesc(packet);
        itemStackTag = Objects.requireNonNullElse(packet.readCompoundNBT(), new CompoundTag());
        icName = packet.readString();
        ifSpec.readDesc(packet);
        compileFormat = packet.readInt();
    }

    @Override
    public ItemStack getItem() {
        ItemStack stack = super.getItem();
        stack.setTag(itemStackTag);
        return stack;
    }

    //region RedstoneGatePart overrides
    @Override
    protected int outputMask(int shape) {
        return ifSpec.getRedstoneOutputMask() | ifSpec.getAnalogOutputMask();
    }

    @Override
    protected int inputMask(int shape) {
        return ifSpec.getRedstoneInputMask() | ifSpec.getAnalogInputMask();
    }

    @Override
    protected int getOutput(int r) {
        if (!ifSpec.isOutput(r)) return 0;

        return switch (ifSpec.getInterfaceType(r)) {
            case NC, BUNDLED -> 0; // Bundled output handled by getBundledOutput
            case REDSTONE -> (simulationContainer.getOutput(r) & 1) != 0 ? 15 : 0;
            case ANALOG -> mostSignificantBit(simulationContainer.getOutput(r));
        };
    }
    //endregion

    //region BundledGatePart overrides
    @Override
    protected int bundledInputMask(int shape) {
        return ifSpec.getBundledInputMask();
    }

    @Override
    protected int bundledOutputMask(int shape) {
        return ifSpec.getBundledOutputMask();
    }

    @Override
    protected @Nullable byte[] getBundledOutput(int r) {
        if (!ifSpec.isOutput(r)) return null;

        return switch (ifSpec.getInterfaceType(r)) {
            case NC, REDSTONE, ANALOG -> null; // Redstone output handled by getOutput
            case BUNDLED -> unpackDigital(null, simulationContainer.getOutput(r)); //TODO reuse an array
        };
    }
    //endregion

    //region IGateRenderKey overrides
    @Override
    public int state2() {
        // TODO Temporary: state2 contains IO details for rendering
        // TODO May let ifSpec pack this?
        int rsMask = ifSpec.getRedstoneInputMask() | ifSpec.getRedstoneOutputMask();
        int analogMask = ifSpec.getAnalogInputMask() | ifSpec.getAnalogOutputMask();
        int bundledMask = ifSpec.getBundledInputMask() | ifSpec.getBundledOutputMask();
        return (rsMask & 0xF) | (analogMask & 0xF) << 4 | (bundledMask & 0xF) << 8;
    }

    @Override
    public String getGateName() {
        return icName;
    }

    @Override
    public boolean hasRuntimeError() {
        return compileFormat != PRFabricationEngine.COMPILE_FORMAT;
    }
    //endregion

    @Override
    protected void gateLogicOnWorldLoad() {
        simulationTimeStart = level().getGameTime() - simulationTimeStart;
    }

    @Override
    protected void gateLogicSetup() {
        if (simulationTimeStart == -1L) {
            simulationTimeStart = level().getGameTime();
            tile().setChanged();
        }
    }

    @Override
    protected void gateLogicOnChange() {

        short[] newInputs = new short[4];
        for (int r = 0; r < 4; r++) newInputs[r] = getModeBasedInput(r);
        int changeMask = simulationContainer.setInputs(newInputs);

        // Schedule update if inputs changed
        if (changeMask != 0) {
            setState(state() & 0xF0 | getModeBasedInputStateMask());
            onInputChange();
            scheduleTick(2);
        }
    }

    @Override
    protected void gateLogicOnScheduledTick() {

        // Push latched inputs into simulation registers
        simulationContainer.pushInputs(0xF);

        // Run simulation
        simulationContainer.simulate();
        int changeMask = simulationContainer.pullOutputs();
        if (changeMask != 0) {
            setState(state() & 0xF | getModeBasedOutputStateMask() << 4);
            onOutputChange(changeMask);
        }

        // Re-check inputs in case they changed during scheduled tick
        gateLogicOnChange();
    }

    @Override
    protected void gateLogicOnTick() {
        if (!level().isClientSide) {
            long simTimeElapsed = level().getGameTime() - simulationTimeStart;

            // Push new simulation time register
            simulationContainer.setSystemTime(simTimeElapsed);
            simulationContainer.pushTime();

            // Run simulation
            simulationContainer.simulate();
            int changeMask = simulationContainer.pullOutputs();
            if (changeMask != 0) {
                setState(state() & 0xF | getModeBasedOutputStateMask() << 4);
                onOutputChange(changeMask);
            }
        }
    }

    private short getModeBasedInput(int r) {
        if (!ifSpec.isInput(r)) return 0;

        return switch (ifSpec.getInterfaceType(r)) {
            case NC       -> 0;
            case REDSTONE -> (short) (getRedstoneInput(r) != 0 ? 0xFFFF : 0);
            case BUNDLED  -> (short) packDigital(getBundledInput(r));
            case ANALOG   -> (short) (1 << getAnalogRedstoneInput(r));
        };
    }

    private int getModeBasedInputStateMask() {
        return getSignalStateMask(simulationContainer::getInput);
    }

    private int getModeBasedOutputStateMask() {
        return getSignalStateMask(simulationContainer::getOutput);
    }

    private int getSignalStateMask(Function<Integer, Short> signalSupplier) {
        int mask = 0;
        for (int r = 0; r < 4; r++) {
            short input = signalSupplier.apply(r);
            mask |= switch (ifSpec.getInterfaceType(r)) {
                case NC -> 0;
                case REDSTONE, BUNDLED -> input != 0 ? 1 : 0;
                case ANALOG -> mostSignificantBit(input) > 0 ? 1 : 0;
            } << r;
        }
        return mask;
    }
}
