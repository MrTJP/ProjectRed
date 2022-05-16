package mrtjp.projectred.fabrication.engine.gates;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.projectred.fabrication.engine.ICTileType;
import mrtjp.projectred.fabrication.engine.IConnectableICTile;
import mrtjp.projectred.fabrication.engine.IRedstoneConnectableICTile;
import net.minecraft.nbt.CompoundNBT;

public abstract class RedstoneGateTile extends GateTile implements IRedstoneConnectableICTile {

    public static final int STATE_PACKET = 5;

    /**
     * Mapped inputs and outputs of the gate.
     * OOOO IIII
     * High nybble is output.
     * Low nybble is input
     */
    private byte gateState = 0;

    public RedstoneGateTile(ICTileType tileType, int renderIndex) {
        super(tileType, renderIndex);
    }

    public void setState(int state) {
        gateState = (byte) state;
    }

    public int getState() {
        return gateState & 0xFF;
    }

    @Override
    public void save(CompoundNBT tag) {
        super.save(tag);
        tag.putByte("state", gateState);
    }

    @Override
    public void load(CompoundNBT tag) {
        super.load(tag);
        gateState = tag.getByte("state");
    }

    @Override
    public void writeDesc(MCDataOutput out) {
        super.writeDesc(out);
        out.writeByte(gateState);
    }

    @Override
    public void readDesc(MCDataInput in) {
        super.readDesc(in);
        gateState = in.readByte();
    }

    @Override
    public void read(MCDataInput in, int key) {
        switch (key) {
            case STATE_PACKET:
                gateState = in.readByte();
                break;
            default:
                super.read(in, key);
        }
    }

    protected void sendStateUpdate() {
        getWriteStream(STATE_PACKET).writeByte(gateState);
    }

    //region IGateRenderKey implementation
    @Override
    public int state() {
        return gateState & 0xFF;
    }
    //endregion

    //region RedstoneGateTile logic override points
    @Override
    protected boolean canGateConnectTo(IConnectableICTile target, int r) {
        if (target instanceof IRedstoneConnectableICTile)
            return canConnectRedstone(r);
        return false;
    }

    protected boolean canConnectRedstone(int r) {
        return canInputRedstone(r) || canOutputRedstone(r);
    }

    protected boolean canOutputRedstone(int r) {
        return (redstoneOutputMask() & 1 << r) != 0;
    }

    protected boolean canInputRedstone(int r) {
        return (redstoneInputMask() & 1 << r) != 0;
    }

    protected int redstoneOutputMask() {
        return 0;
    }

    protected int redstoneInputMask() {
        return 0;
    }
    //endregion
}
