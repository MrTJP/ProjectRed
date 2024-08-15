package mrtjp.projectred.fabrication.engine;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import net.minecraft.nbt.CompoundTag;

import java.util.Arrays;
import java.util.Set;

public class InterfaceSpec {

    private final ICInterfaceType[] sideInterfaces = new ICInterfaceType[4];
    private int redstoneMask = 0; // OOOO IIII
    private int bundledMask = 0; // OOOO IIII
    private int analogMask = 0; // OOOO IIII

    {
        Arrays.fill(sideInterfaces, ICInterfaceType.NC);
    }

    //region Save/load
    public void save(CompoundTag tag) {
        for (int r = 0; r < 4; r++) {
            tag.putByte("if" + r, (byte) sideInterfaces[r].getId());
        }
        tag.putByte("rmask", (byte) redstoneMask);
        tag.putByte("bmask", (byte) bundledMask);
        tag.putByte("amask", (byte) analogMask);
    }

    public void load(CompoundTag tag) {
        for (int r = 0; r < 4; r++) {
            sideInterfaces[r] = ICInterfaceType.fromId(tag.getByte("if" + r) & 0xFF);
        }
        redstoneMask = tag.getByte("rmask") & 0xFF;
        bundledMask = tag.getByte("bmask") & 0xFF;
        analogMask = tag.getByte("amask") & 0xFF;
    }

    public void writeDesc(MCDataOutput packet) {
        for (int r = 0; r < 4; r++) {
            packet.writeByte(sideInterfaces[r].getId());
        }
        packet.writeByte(bundledMask);
        packet.writeByte(redstoneMask);
        packet.writeByte(analogMask);
    }

    public void readDesc(MCDataInput packet) {
        for (int r = 0; r < 4; r++) {
            sideInterfaces[r] = ICInterfaceType.fromId(packet.readByte() & 0xFF);
        }
        bundledMask = packet.readByte();
        redstoneMask = packet.readByte();
        analogMask = packet.readByte();
    }
    //endregion

    public void setFromIOTiles(Set<IIOConnectionTile> ioTiles) {

        Arrays.fill(sideInterfaces, ICInterfaceType.NC);
        bundledMask = 0;
        redstoneMask = 0;
        analogMask = 0;

        for (IIOConnectionTile t : ioTiles) {
            int side = t.getIOSide();
            ICInterfaceType type = t.getInterfaceType();
            int dir = t.isInputIOMode() ? 0x1 : 0x10;

            sideInterfaces[side] = type;

            switch (type) {
                case REDSTONE -> redstoneMask |= dir << side;
                case BUNDLED -> bundledMask |= dir << side;
                case ANALOG -> analogMask |= dir << side;
            }
        }
    }

    public ICInterfaceType getInterfaceType(int side) {
        return sideInterfaces[side];
    }

    public int getInputMask() {
        return (redstoneMask | bundledMask | analogMask) & 0xF;
    }

    public int getOutputMask() {
        return (redstoneMask | bundledMask | analogMask) >> 4 & 0xF;
    }

    public boolean isInput(int side) {
        return ((redstoneMask | bundledMask | analogMask) & 1 << side) != 0;
    }

    public boolean isOutput(int side) {
        return ((redstoneMask | bundledMask | analogMask) & 0x10 << side) != 0;
    }

    public int getRedstoneInputMask() {
        return redstoneMask & 0xF;
    }

    public int getRedstoneOutputMask() {
        return redstoneMask >> 4 & 0xF;
    }

    public int getBundledInputMask() {
        return bundledMask & 0xF;
    }

    public int getBundledOutputMask() {
        return bundledMask >> 4 & 0xF;
    }

    public int getAnalogInputMask() {
        return analogMask & 0xF;
    }

    public int getAnalogOutputMask() {
        return analogMask >> 4 & 0xF;
    }

    //region Utilities
    public void saveTo(CompoundTag tag, String key) {
        CompoundTag tag1 = new CompoundTag();
        save(tag1);
        tag.put(key, tag1);
    }

    public void loadFrom(CompoundTag tag, String key) {
        CompoundTag tag1 = tag.getCompound(key);
        load(tag1);
    }

    public static InterfaceSpec createFrom(CompoundTag tag, String key) {
        CompoundTag tag1 = tag.getCompound(key);
        InterfaceSpec spec = new InterfaceSpec();
        spec.load(tag1);
        return spec;
    }

    //endregion
}
