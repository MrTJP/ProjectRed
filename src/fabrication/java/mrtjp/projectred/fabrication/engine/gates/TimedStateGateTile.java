package mrtjp.projectred.fabrication.engine.gates;

import mrtjp.fengine.simulate.ByteRegister;
import mrtjp.projectred.fabrication.engine.ICTileType;
import net.minecraft.nbt.CompoundNBT;

public abstract class TimedStateGateTile extends SidedRedstoneGateTile {

    protected int stateReg = -1;
    protected int timeReg3 = -1;
    protected int timeReg2 = -1;
    protected int timeReg1 = -1;
    protected int timeReg0 = -1;

    public TimedStateGateTile(ICTileType tileType, int renderIndex) {
        super(tileType, renderIndex);
    }

    @Override
    public void save(CompoundNBT tag) {
        super.save(tag);
        tag.putInt("regS", stateReg);
        tag.putInt("regT3", timeReg3);
        tag.putInt("regT2", timeReg2);
        tag.putInt("regT1", timeReg1);
        tag.putInt("regT0", timeReg0);
    }

    @Override
    public void load(CompoundNBT tag) {
        super.load(tag);
        stateReg = tag.getInt("regS");
        timeReg3 = tag.getInt("regT3");
        timeReg2 = tag.getInt("regT2");
        timeReg1 = tag.getInt("regT1");
        timeReg0 = tag.getInt("regT0");
    }

    @Override
    protected void clearRegisterIds() {
        super.clearRegisterIds();
        stateReg = -1;
        timeReg3 = -1;
        timeReg2 = -1;
        timeReg1 = -1;
        timeReg0 = -1;
    }

    //region FETile overrides

    @Override
    public void allocate(Allocator allocator) {
        super.allocate(allocator);
        stateReg = allocator.allocRegisterID();
        timeReg3 = allocator.allocRegisterID();
        timeReg2 = allocator.allocRegisterID();
        timeReg1 = allocator.allocRegisterID();
        timeReg0 = allocator.allocRegisterID();
    }

    @Override
    public void consumeRemaps(RemapProvider remapProvider) {
        super.consumeRemaps(remapProvider);
        stateReg = remapProvider.getRemappedRegisterID(stateReg);
        timeReg3 = remapProvider.getRemappedRegisterID(timeReg3);
        timeReg2 = remapProvider.getRemappedRegisterID(timeReg2);
        timeReg1 = remapProvider.getRemappedRegisterID(timeReg1);
        timeReg0 = remapProvider.getRemappedRegisterID(timeReg0);
    }

    @Override
    public void collect(Collector collector) {
        super.collect(collector);

        collector.addRegister(stateReg, new ByteRegister());
        collector.addRegister(timeReg3, new ByteRegister());
        collector.addRegister(timeReg2, new ByteRegister());
        collector.addRegister(timeReg1, new ByteRegister());
        collector.addRegister(timeReg0, new ByteRegister());
    }

    //endregion
}
