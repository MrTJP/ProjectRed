package mrtjp.projectred.fabrication.engine;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;

import java.util.Arrays;
import java.util.Set;

public class InterfaceSpec {

    // Empty instance
    public static final InterfaceSpec EMPTY = new InterfaceSpec(
            new ICInterfaceType[]{ICInterfaceType.NC, ICInterfaceType.NC, ICInterfaceType.NC, ICInterfaceType.NC},
            0, 0, 0);

    private final ICInterfaceType[] sideInterfaces;
    private final int redstoneMask; // OOOO IIII
    private final int bundledMask; // OOOO IIII
    private final int analogMask; // OOOO IIII

    //region Codecs
    public static final Codec<ICInterfaceType> INTERFACE_TYPE_CODEC = Codec.BYTE.xmap(
            id -> ICInterfaceType.fromId(id & 0xFF),
            type -> (byte) type.getId());

    public static final StreamCodec<RegistryFriendlyByteBuf, ICInterfaceType> INTERFACE_TYPE_STREAM_CODEC = StreamCodec.of(
            (buf, type) -> buf.writeByte(type.getId()),
            buf -> ICInterfaceType.fromId(buf.readByte() & 0xFF));

    public static final Codec<InterfaceSpec> CODEC = RecordCodecBuilder.create(instance -> instance.group(
            Codec.list(INTERFACE_TYPE_CODEC).fieldOf("sideInterfaces").forGetter(spec -> Arrays.asList(spec.sideInterfaces)),
            Codec.INT.fieldOf("redstoneMask").forGetter(spec -> spec.redstoneMask),
            Codec.INT.fieldOf("bundledMask").forGetter(spec -> spec.bundledMask),
            Codec.INT.fieldOf("analogMask").forGetter(spec -> spec.analogMask)
    ).apply(instance, (sideInterfaces, redstoneMask, bundledMask, analogMask) ->
            new InterfaceSpec(sideInterfaces.toArray(new ICInterfaceType[4]), redstoneMask, bundledMask, analogMask)));

    public static final StreamCodec<RegistryFriendlyByteBuf, InterfaceSpec> STREAM_CODEC = StreamCodec.composite(
            INTERFACE_TYPE_STREAM_CODEC.apply(ByteBufCodecs.list(4)), spec -> Arrays.asList(spec.sideInterfaces),
            ByteBufCodecs.INT, spec -> spec.redstoneMask,
            ByteBufCodecs.INT, spec -> spec.bundledMask,
            ByteBufCodecs.INT, spec -> spec.analogMask,
            (sideInterfaces, redstoneMask, bundledMask, analogMask) ->
                    new InterfaceSpec(sideInterfaces.toArray(new ICInterfaceType[4]), redstoneMask, bundledMask, analogMask));
    //endregion

    // Constructor for immutable creation
    private InterfaceSpec(ICInterfaceType[] sideInterfaces, int redstoneMask, int bundledMask, int analogMask) {
        this.sideInterfaces = Arrays.copyOf(sideInterfaces, 4);
        this.redstoneMask = redstoneMask;
        this.bundledMask = bundledMask;
        this.analogMask = analogMask;
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

    public static InterfaceSpec load(CompoundTag tag) {
        ICInterfaceType[] sideInterfaces = new ICInterfaceType[4];
        for (int r = 0; r < 4; r++) {
            sideInterfaces[r] = ICInterfaceType.fromId(tag.getByte("if" + r) & 0xFF);
        }
        int redstoneMask = tag.getByte("rmask") & 0xFF;
        int bundledMask = tag.getByte("bmask") & 0xFF;
        int analogMask = tag.getByte("amask") & 0xFF;

        return new InterfaceSpec(sideInterfaces, redstoneMask, bundledMask, analogMask);
    }

    public void writeDesc(MCDataOutput packet) {
        for (int r = 0; r < 4; r++) {
            packet.writeByte(sideInterfaces[r].getId());
        }
        packet.writeByte(bundledMask);
        packet.writeByte(redstoneMask);
        packet.writeByte(analogMask);
    }

    public static InterfaceSpec readDesc(MCDataInput packet) {
        ICInterfaceType[] sideInterfaces = new ICInterfaceType[4];
        for (int r = 0; r < 4; r++) {
            sideInterfaces[r] = ICInterfaceType.fromId(packet.readByte() & 0xFF);
        }
        int bundledMask = packet.readByte();
        int redstoneMask = packet.readByte();
        int analogMask = packet.readByte();

        return new InterfaceSpec(sideInterfaces, redstoneMask, bundledMask, analogMask);
    }
    //endregion

    public static InterfaceSpec fromIOTiles(Set<IIOConnectionTile> ioTiles) {
        ICInterfaceType[] sideInterfaces = new ICInterfaceType[4];
        Arrays.fill(sideInterfaces, ICInterfaceType.NC);
        int bundledMask = 0;
        int redstoneMask = 0;
        int analogMask = 0;

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

        return new InterfaceSpec(sideInterfaces, redstoneMask, bundledMask, analogMask);
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

    public static InterfaceSpec loadFrom(CompoundTag tag, String key) {
        CompoundTag tag1 = tag.getCompound(key);
        return load(tag1);
    }

    public static InterfaceSpec createFrom(CompoundTag tag, String key) {
        return loadFrom(tag, key);
    }
    //endregion
}
