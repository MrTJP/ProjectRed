package mrtjp.projectred.fabrication.engine.gates;

import mrtjp.fengine.simulate.ByteRegister;
import net.minecraft.nbt.CompoundTag;

public abstract class InternalStateGateTile extends SidedRedstoneGateTile {

    protected int stateReg = -1;

    public InternalStateGateTile(ICGateTileType gateType) {
        super(gateType);
    }

    @Override
    public void save(CompoundTag tag) {
        super.save(tag);
        tag.putInt("regS", stateReg);
    }

    @Override
    public void load(CompoundTag tag) {
        super.load(tag);
        stateReg = tag.getInt("regS");
    }

    @Override
    protected void clearRegisterIds() {
        super.clearRegisterIds();
        stateReg = -1;
    }

    //region FETile overrides

    @Override
    public void allocate(Allocator allocator) {
        super.allocate(allocator);
        stateReg = allocator.allocRegisterID();
    }

    @Override
    public void consumeRemaps(RemapProvider remapProvider) {
        super.consumeRemaps(remapProvider);
        stateReg = remapProvider.getRemappedRegisterID(stateReg);
    }

    @Override
    public void collect(Collector collector) {
        super.collect(collector);
        collector.addRegister(stateReg, new ByteRegister());
    }

    //endregion
}
