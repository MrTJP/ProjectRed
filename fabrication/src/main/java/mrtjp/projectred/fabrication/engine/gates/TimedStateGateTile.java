package mrtjp.projectred.fabrication.engine.gates;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.fengine.simulate.ByteRegister;
import mrtjp.projectred.fabrication.engine.ICSimulationContainer;
import net.minecraft.nbt.CompoundTag;

import java.util.Arrays;

public abstract class TimedStateGateTile extends SidedRedstoneGateTile {

    public static final int STATE2_PACKET = 6;

    protected int stateReg = -1;
    protected int[] timeRegs = new int[8];

    private byte state2 = 0;

    {
        Arrays.fill(timeRegs, -1);
    }

    public TimedStateGateTile(ICGateTileType gateType) {
        super(gateType);
    }

    public int getState2() {
        return state2 & 0xFF;
    }

    //region BaseTile overrides
    @Override
    public void save(CompoundTag tag) {
        super.save(tag);
        tag.putByte("state2", state2);
        tag.putInt("regS", stateReg);
        tag.putIntArray("reg_time", timeRegs);
    }

    @Override
    public void load(CompoundTag tag) {
        super.load(tag);
        state2 = tag.getByte("state2");
        stateReg = tag.getInt("regS");
        timeRegs = tag.getIntArray("reg_time");
    }

    @Override
    public void writeDesc(MCDataOutput out) {
        super.writeDesc(out);
        out.writeByte(state2);
    }

    @Override
    public void readDesc(MCDataInput in) {
        super.readDesc(in);
        state2 = in.readByte();
    }

    @Override
    public void read(MCDataInput in, int key) {
        switch (key) {
            case STATE2_PACKET -> state2 = in.readByte();
            default -> super.read(in, key);
        }
    }

    protected void sendState2Update() {
        getWriteStream(STATE2_PACKET).writeByte(state2);
    }

    @Override
    public void onSimRegistersChanged(int rMask, ICSimulationContainer container) {
        super.onSimRegistersChanged(rMask, container);
        byte oldS2 = state2;
        state2 = container.pullRegisterValue(stateReg);
        if (oldS2 != state2) {
            sendState2Update();
        }
    }
    //endregion

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
