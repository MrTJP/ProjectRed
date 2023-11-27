package mrtjp.projectred.fabrication.part;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.fengine.api.ICFlatMap;
import mrtjp.projectred.core.BundledSignalsLib;
import mrtjp.projectred.fabrication.engine.ICInterfaceType;
import mrtjp.projectred.fabrication.engine.ICSimulationContainer;
import mrtjp.projectred.fabrication.engine.InterfaceSpec;
import mrtjp.projectred.fabrication.engine.PRFabricationEngine;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.part.BundledGatePart;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.LOGGER;
import static mrtjp.projectred.fabrication.editor.EditorDataUtils.*;

public class FabricatedGatePart extends BundledGatePart {

    private final ICSimulationContainer simulationContainer = new ICSimulationContainer();
    private String icName = "untitled";
    private final InterfaceSpec ifSpec = new InterfaceSpec();

    private long simulationTimeStart = -1L;

    public FabricatedGatePart() {
        super(GateType.FABRICATED_GATE);
    }

    @Override
    public void preparePlacement(Player player, BlockPos pos, int side) {
        super.preparePlacement(player, pos, side);

        if (player.level.isClientSide) return;

        ItemStack stack = player.getItemInHand(InteractionHand.MAIN_HAND); // TODO handle offhand
        if (stack.isEmpty() || !stack.hasTag()) {
            LOGGER.warn("Gate placement issue: no NBT on gate item");
            return;
        }

        CompoundTag tag = stack.getTag();
        icName = tag.getString(KEY_IC_NAME);
        ICFlatMap flatMap = PRFabricationEngine.instance.deserializeFlatMap(tag.getString(KEY_FLAT_MAP));
        simulationContainer.setFlatMap(flatMap);

        ifSpec.loadFrom(tag, KEY_IO_SPEC);
    }

    @Override
    public void save(CompoundTag tag) {
        super.save(tag);
        tag.putString("ic_name", icName);
        tag.putLong("sim_time", level().getGameTime() - simulationTimeStart);
        simulationContainer.save(tag);
        ifSpec.saveTo(tag, "io_spec");
    }

    @Override
    public void load(CompoundTag tag) {
        super.load(tag);
        icName = tag.getString("ic_name");
        simulationTimeStart = tag.getLong("sim_time");
        simulationContainer.load(tag);
        ifSpec.loadFrom(tag, "io_spec");
    }

    @Override
    public void writeDesc(MCDataOutput packet) {
        super.writeDesc(packet);
        packet.writeString(icName);
        ifSpec.writeDesc(packet);
    }

    @Override
    public void readDesc(MCDataInput packet) {
        super.readDesc(packet);
        icName = packet.readString();
        ifSpec.readDesc(packet);
    }

    @Override
    public ItemStack getItem() {
        return super.getItem(); //TODO set nbt on this item
    }

    //region RedstoneGatePart overrides
    @Override
    protected int outputMask(int shape) {
        return ifSpec.getRedstoneOutputMask();
    }

    @Override
    protected int inputMask(int shape) {
        return ifSpec.getRedstoneInputMask();
    }

    @Override
    protected int getOutput(int r) {
        if (!ifSpec.isOutput(r)) return 0;

        return switch (ifSpec.getInterfaceType(r)) {
            case NC, BUNDLED -> 0; // Bundled output handled by getBundledOutput
            case REDSTONE -> (simulationContainer.getOutput(r) & 1) != 0 ? 15 : 0;
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
    protected byte[] getBundledOutput(int r) {
        if (!ifSpec.isOutput(r)) return null;

        return switch (ifSpec.getInterfaceType(r)) {
            case NC, REDSTONE -> null; // Redstone output handled by getOutput
            case BUNDLED -> BundledSignalsLib.unpackDigital(null, simulationContainer.getOutput(r)); //TODO reuse an array
        };
    }
    //endregion

    //region IGateRenderKey overrides
    @Override
    public int state2() {
        // TODO Temporary: state2 contains IO details for rendering
        // TODO May let ifSpec pack this?
        int rsMask = ifSpec.getRedstoneInputMask() | ifSpec.getRedstoneOutputMask();
        int analogMask = 0; //TODO
        int bundledMask = ifSpec.getBundledInputMask() | ifSpec.getBundledOutputMask();
        return (rsMask & 0xF) | (analogMask & 0xF) << 4 | (bundledMask & 0xF) << 8;
    }

    @Override
    public String getGateName() {
        return icName;
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
            setState(state() & 0xF0 | simulationContainer.inputMask());
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
            setState(state() & 0xF | simulationContainer.outputMask() << 4);
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
                setState(state() & 0xF | simulationContainer.outputMask() << 4);
                onOutputChange(changeMask);
            }
        }
    }

    private short getModeBasedInput(int r) {
        if (!ifSpec.isInput(r)) return 0;

        return switch (ifSpec.getInterfaceType(r)) {
            case NC       -> 0;
            case REDSTONE -> (short) (getRedstoneInput(r) != 0 ? 0xFFFF : 0);
            case BUNDLED  -> (short) BundledSignalsLib.packDigital(getBundledInput(r));
        };
    }
}
