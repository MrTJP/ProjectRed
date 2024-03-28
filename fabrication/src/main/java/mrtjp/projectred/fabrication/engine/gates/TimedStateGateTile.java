package mrtjp.projectred.fabrication.engine.gates;

import mrtjp.fengine.simulate.ByteRegister;
import net.minecraft.nbt.CompoundTag;

import java.util.Arrays;

public abstract class TimedStateGateTile extends SidedRedstoneGateTile {

    protected int stateReg = -1;
    protected int[] timeRegs = new int[8];

    {
        Arrays.fill(timeRegs, -1);
    }

    public TimedStateGateTile(ICGateTileType gateType) {
        super(gateType);
    }

    @Override
    public void save(CompoundTag tag) {
        super.save(tag);
        tag.putInt("regS", stateReg);
        tag.putIntArray("reg_time", timeRegs);
    }

    @Override
    public void load(CompoundTag tag) {
        super.load(tag);
        stateReg = tag.getInt("regS");
        timeRegs = tag.getIntArray("reg_time");
    }

    @Override
    protected void clearRegisterIds() {
        super.clearRegisterIds();
        stateReg = -1;
        Arrays.fill(timeRegs, -1);
    }

    //region FETile overrides

    @Override
    public void allocate(Allocator allocator) {
        super.allocate(allocator);
        stateReg = allocator.allocRegisterID();
        for (int i = 0; i < 8; i++) {
            timeRegs[i] = allocator.allocRegisterID();
        }
    }

    @Override
    public void consumeRemaps(RemapProvider remapProvider) {
        super.consumeRemaps(remapProvider);
        stateReg = remapProvider.getRemappedRegisterID(stateReg);
        for (int i = 0; i < 8; i++) {
            timeRegs[i] = remapProvider.getRemappedRegisterID(timeRegs[i]);
        }
    }

    @Override
    public void collect(Collector collector) {
        super.collect(collector);
        collector.addRegister(stateReg, new ByteRegister());
        for (int timeReg : timeRegs) {
            collector.addRegister(timeReg, new ByteRegister());
        }
    }

    //endregion
}
