package mrtjp.projectred.fabrication.tile;

import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.power.ILowLoadMachine;
import mrtjp.projectred.core.power.ILowLoadPowerLine;
import mrtjp.projectred.core.power.PowerConductor;
import mrtjp.projectred.core.tile.BasePoweredTile;
import mrtjp.projectred.fabrication.block.FabricationMachineBlock;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

public abstract class FabricationMachineTile extends BasePoweredTile implements ILowLoadMachine {

    protected final PowerConductor conductor = new PowerConductor(this, 0.01, 160);

    private int chargeFlow = 0;
    private boolean isWorking = false;
    private boolean isCharged = false;
    private int remainingWork = 0;
    private int totalWork = 0;

    public FabricationMachineTile(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
    }

    @Override
    public void saveToNBT(CompoundTag tag) {
        super.saveToNBT(tag);
        conductor.save(tag);
        tag.putBoolean("isWorking", isWorking);
        tag.putBoolean("isCharged", isCharged);
    }

    @Override
    public void loadFromNBT(CompoundTag tag) {
        super.loadFromNBT(tag);
        conductor.load(tag);
        isWorking = tag.getBoolean("isWorking");
        isCharged = tag.getBoolean("isCharged");
    }

    @Override
    public BlockState storeBlockState(BlockState defaultState) {
        return super.storeBlockState(defaultState)
                .setValue(FabricationMachineBlock.CHARGED, isCharged)
                .setValue(FabricationMachineBlock.WORKING, isWorking);
    }

    @Override
    public void tick() {
        if (getLevel().isClientSide) return;

        boolean wasCharged = isCharged;
        boolean wasWorking = isWorking;

        conductor.tick();

        chargeFlow <<= 1;
        if (canConductorWork()) chargeFlow |= 1;
        isCharged = canConductorWork();

        if (!isWorking) {
            if (canStartWork()) {
                isWorking = true;
                remainingWork = totalWork = startWork();
            }

        } else {
            int workDone = tickWork(remainingWork);
            remainingWork -= workDone;
            if (remainingWork <= 0) {
                isWorking = false;
                remainingWork = 0;
                totalWork = 0;
                finishWork();
            }
        }

        if (isCharged != wasCharged || isWorking != wasWorking) {
            pushBlockState();
        }
    }

    @Override
    public boolean canConnectPart(IConnectable part, int s, int edgeRot) {
        if (part instanceof ILowLoadMachine) return true;
        if (part instanceof ILowLoadPowerLine) return true;

        return false;
    }

    @Override
    public PowerConductor getConductor(int dir) {
        return conductor;
    }

    @Override
    public int getConductorCharge() {
        return (int) (conductor.getVoltage() * 10);
    }

    @Override
    public int getConductorFlow() {
        return chargeFlow;
    }

    @Override
    public boolean canConductorWork() {
        return getConductorCharge() > 600;
    }

    protected void cancelWorkIfNeeded() {
        if (isWorking && !canStartWork()) {
            isWorking = false;
            remainingWork = 0;
            totalWork = 0;
            pushBlockState();
        }
    }

    public int getRemainingWork() {
        return remainingWork;
    }

    public int getTotalWork() {
        return totalWork;
    }

    protected abstract boolean canStartWork();
    protected abstract int startWork();
    protected abstract int tickWork(int remainingWork);
    protected abstract void finishWork();
}
