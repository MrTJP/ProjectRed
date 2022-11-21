package mrtjp.projectred.fabrication.part;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.fengine.api.ICFlatMap;
import mrtjp.projectred.core.BundledSignalsLib;
import mrtjp.projectred.fabrication.engine.ICSimulationContainer;
import mrtjp.projectred.fabrication.engine.PRFabricationEngine;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.part.BundledGatePart;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.LOGGER;

public class FabricatedGatePart extends BundledGatePart {

    private final ICSimulationContainer simulationContainer = new ICSimulationContainer();
    private String icName = "untitled";
    private byte bundledMask = 0;
    private byte redstoneMask = 0;

    private long simulationTimeStart = -1L;

    public FabricatedGatePart() {
        super(GateType.FABRICATED_GATE);
    }

    @Override
    public void preparePlacement(Player player, BlockPos pos, int side) {
        super.preparePlacement(player, pos, side);

        ItemStack stack = player.getItemInHand(InteractionHand.MAIN_HAND); // TODO handle offhand
        if (stack.isEmpty() || !stack.hasTag()) {
            LOGGER.warn("Gate placement issue: no NBT on gate item");
            return;
        }

        CompoundTag tag = stack.getTag();
        icName = tag.getString("ic_name");
        ICFlatMap flatMap = PRFabricationEngine.instance.deserializeFlatMap(tag.getString("flatMap"));
        simulationContainer.setFlatMap(flatMap);
        bundledMask = tag.getByte("bmask");
        redstoneMask = tag.getByte("rmask");
    }

    @Override
    public void save(CompoundTag tag) {
        super.save(tag);
        tag.putString("ic_name", icName);
        tag.putByte("bmask", bundledMask);
        tag.putByte("rmask", redstoneMask);
        tag.putLong("sim_time", level().getGameTime() - simulationTimeStart);
        simulationContainer.save(tag);
    }

    @Override
    public void load(CompoundTag tag) {
        super.load(tag);
        icName = tag.getString("ic_name");
        bundledMask = tag.getByte("bmask");
        redstoneMask = tag.getByte("rmask");
        simulationTimeStart = tag.getLong("sim_time");
        simulationContainer.load(tag);
    }

    @Override
    public void writeDesc(MCDataOutput packet) {
        super.writeDesc(packet);
        packet.writeString(icName);
        packet.writeByte(bundledMask);
        packet.writeByte(redstoneMask);
    }

    @Override
    public void readDesc(MCDataInput packet) {
        super.readDesc(packet);
        icName = packet.readString();
        bundledMask = packet.readByte();
        redstoneMask = packet.readByte();
    }

    @Override
    public ItemStack getItem() {
        return super.getItem(); //TODO set nbt on this item
    }

    //region RedstoneGatePart overrides
    @Override
    protected int outputMask(int shape) {
        return (redstoneMask >> 4) & 0xF;
    }

    @Override
    protected int inputMask(int shape) {
        return redstoneMask & 0xF;
    }

    @Override
    protected int getOutput(int r) {
        if ((outputMask(shape()) & 1 << r) != 0) {
            return 0;
        }

        // TODO interpret analog
        return (simulationContainer.getOutput(r) & 1) != 0 ? 15 : 0;
    }

    //endregion

    //region BundledGatePart overrides
    @Override
    protected int bundledInputMask(int shape) {
        return bundledMask & 0xF;
    }

    @Override
    protected int bundledOutputMask(int shape) {
        return (bundledMask >> 4) & 0xF;
    }

    @Override
    protected byte[] getBundledOutput(int r) {
        return (bundledOutputMask(shape()) & (1 << r)) != 0 ?
                BundledSignalsLib.unpackDigital(null, simulationContainer.getOutput(r)) : null; //TODO reuse an array
    }
    //endregion

    //region IGateRenderKey overrides

    @Override
    public int state2() {
        // TODO Temporary: Mask used to render Bundled side conns by FabricatedGateRenderer
        return bundledMask;
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

        // Latch new input states
        short[] newInputs = new short[4];
        for (int r = 0; r < 4; r++) newInputs[r] = getModeBasedInput(r);
        int changeMask = simulationContainer.setInputs(newInputs);

        // Schedule update if inputs changed
        if (changeMask != 0) {
            // TODO set gate state for client rendering here
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
            // TODO set gate state for client rendering here
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
                // TODO set gate state for client rendering here
                onOutputChange(changeMask);
            }
        }
    }

    private short getModeBasedInput(int r) {
        if ((bundledInputMask(shape()) & (1 << r)) == 0) return 0;
        return (short) BundledSignalsLib.packDigital(getBundledInput(r));
    }
}
