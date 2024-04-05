package mrtjp.projectred.fabrication.engine.gates;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.fengine.simulate.ByteRegister;
import mrtjp.projectred.fabrication.engine.ICSimulationContainer;
import net.minecraft.nbt.CompoundTag;

public abstract class InternalStateGateTile extends SidedRedstoneGateTile {

    public static final int STATE2_PACKET = 6;

    protected int stateReg = -1;
    private byte state2 = 0;

    public InternalStateGateTile(ICGateTileType gateType) {
        super(gateType);
    }

    public int getState2() {
        return state2 & 0xFF;
    }

    @Override
    public void save(CompoundTag tag) {
        super.save(tag);
        tag.putByte("state2", state2);
        tag.putInt("regS", stateReg);
    }

    @Override
    public void load(CompoundTag tag) {
        super.load(tag);
        state2 = tag.getByte("state2");
        stateReg = tag.getInt("regS");
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
