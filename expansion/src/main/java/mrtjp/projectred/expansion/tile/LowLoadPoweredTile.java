package mrtjp.projectred.expansion.tile;

import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.power.ILowLoadMachine;
import mrtjp.projectred.core.power.ILowLoadPowerLine;
import mrtjp.projectred.core.power.PowerConductor;
import mrtjp.projectred.core.tile.BasePoweredTile;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

public abstract class LowLoadPoweredTile extends BasePoweredTile implements ILowLoadMachine {

    protected final PowerConductor conductor = new PowerConductor(this, 0.01, 160);

    private int chargeFlow = 0;

    public LowLoadPoweredTile(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
    }

    @Override
    public void saveToNBT(CompoundTag tag) {
        super.saveToNBT(tag);
        conductor.save(tag);
        tag.putInt("chargeFlow", chargeFlow);
    }

    @Override
    public void loadFromNBT(CompoundTag tag) {
        super.loadFromNBT(tag);
        conductor.load(tag);
        chargeFlow = tag.getInt("chargeFlow");
    }

    @Override
    public void tick() {
        if (getLevel().isClientSide) return;
        conductor.tick();

        chargeFlow <<= 1;
        if (canConductorWork()) chargeFlow |= 1;
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
}
