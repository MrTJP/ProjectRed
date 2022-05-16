package mrtjp.projectred.fabrication.engine.gates;

import mrtjp.fengine.simulate.ByteRegister;
import mrtjp.projectred.fabrication.engine.ICTileType;
import net.minecraft.nbt.CompoundNBT;

public abstract class InternalStateGateTile extends SidedRedstoneGateTile {

    protected int stateReg = -1;

    public InternalStateGateTile(ICTileType tileType, int renderIndex) {
        super(tileType, renderIndex);
    }

    @Override
    public void save(CompoundNBT tag) {
        super.save(tag);
        tag.putInt("regS", stateReg);
    }

    @Override
    public void load(CompoundNBT tag) {
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
