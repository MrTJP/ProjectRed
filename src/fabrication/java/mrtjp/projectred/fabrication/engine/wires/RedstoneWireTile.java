package mrtjp.projectred.fabrication.engine.wires;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.fengine.api.IPathFinderManifest;
import mrtjp.projectred.fabrication.engine.ICSimulationContainer;
import mrtjp.projectred.fabrication.engine.ICTileType;
import mrtjp.projectred.fabrication.engine.IConnectableICTile;
import mrtjp.projectred.fabrication.engine.IRedstoneConnectableICTile;
import mrtjp.projectred.transmission.WireType;
import net.minecraft.nbt.CompoundNBT;

import java.util.HashSet;
import java.util.Set;

public abstract class RedstoneWireTile extends WireTile implements IRedstoneConnectableICTile {

    private static final int PACKET_SIGNAL = 2;

    protected final Set<Integer> inputRegisters = new HashSet<>();

    protected byte signal = 0;

    public RedstoneWireTile(ICTileType tileType, WireType renderType) {
        super(tileType, renderType);
    }
    @Override
    public void save(CompoundNBT tag) {
        super.save(tag);
        tag.putByte("signal", signal);
        tag.putByte("numInputReg", (byte) inputRegisters.size());

        int i = 0;
        for (Integer inputReg : inputRegisters) tag.putInt("inR" + (i++), inputReg);
    }

    @Override
    public void load(CompoundNBT tag) {
        super.load(tag);
        signal = tag.getByte("signal");

        int numInputReg = tag.getByte("numInputReg") & 0xFF;
        for (int i = 0; i < numInputReg; i++) inputRegisters.add(tag.getInt("inR" + i));
    }

    @Override
    public void writeDesc(MCDataOutput out) {
        super.writeDesc(out);
        out.writeByte(signal);
    }

    @Override
    public void readDesc(MCDataInput in) {
        super.readDesc(in);
        signal = in.readByte();
    }

    @Override
    public void read(MCDataInput in, int key) {
        switch (key) {
            case PACKET_SIGNAL:
                signal = in.readByte();
                break;
            default:
                super.read(in, key);
        }
    }

    protected void sendSignalUpdate() {
        getWriteStream(PACKET_SIGNAL).writeByte(signal);
    }

    @Override
    public boolean canConnectTo(IConnectableICTile target, int towardsDir) {
        if (target instanceof IRedstoneConnectableICTile)
            return true;

        return false;
    }

    @Override
    public void onSimRegistersChanged(int rMask, ICSimulationContainer container) {

        byte oldSignal = signal;
        signal = 0;

        for (Integer inputReg : inputRegisters) {
            boolean isHigh = container.pullRegisterValue(inputReg) > 0;
            signal = isHigh ? (byte) 255 : 0;
            if (isHigh) break;
        }

        if (oldSignal != signal) sendSignalUpdate();
    }


    @Override
    public void searchManifest(IPathFinderManifest manifest) {
        inputRegisters.clear();
        inputRegisters.addAll(manifest.getOutputRegisters());
    }

    @Override
    public void consumeRemaps(RemapProvider remapProvider) {

        Set<Integer> remappedInputs = new HashSet<>();
        for (Integer inputReg : inputRegisters) {
            int remapped = remapProvider.getRemappedRegisterID(inputReg);
            remappedInputs.add(remapped);
        }

        inputRegisters.clear();
        inputRegisters.addAll(remappedInputs);
    }
}
