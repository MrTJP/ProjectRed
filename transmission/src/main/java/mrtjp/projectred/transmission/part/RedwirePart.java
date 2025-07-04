package mrtjp.projectred.transmission.part;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.vec.Rotation;
import codechicken.multipart.api.RedstoneInteractions;
import codechicken.multipart.api.part.MultiPart;
import codechicken.multipart.api.part.redstone.FaceRedstonePart;
import codechicken.multipart.api.part.redstone.RedstonePart;
import codechicken.multipart.trait.extern.RedstoneTile;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.FaceLookup;
import mrtjp.projectred.core.RedstoneFaceLookup;
import mrtjp.projectred.core.RedstonePropagator;
import mrtjp.projectred.core.part.*;
import mrtjp.projectred.transmission.WireType;
import net.minecraft.core.Direction;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;

import static mrtjp.projectred.core.RedstonePropagator.FORCE;
import static mrtjp.projectred.core.RedstonePropagator.RISING;

public abstract class RedwirePart extends BaseFaceWirePart implements IRedstonePropagationPart, IPropagationFacePart, FaceRedstonePart, IRedwirePart {

    private static final int KEY_SIGNAL = 10;

    private byte signal = 0;

    public RedwirePart(WireType wireType) {
        super(wireType);
    }

    //region Save/Load
    @Override
    public void save(CompoundTag tag, HolderLookup.Provider registries) {
        super.save(tag, registries);
        tag.putByte("signal", signal);
    }

    @Override
    public void load(CompoundTag tag, HolderLookup.Provider registries) {
        super.load(tag, registries);
        signal = tag.getByte("signal");
    }

    @Override
    public void writeDesc(MCDataOutput packet) {
        super.writeDesc(packet);
        packet.writeByte(signal);
    }

    @Override
    public void readDesc(MCDataInput packet) {
        super.readDesc(packet);
        signal = packet.readByte();
    }
    //endregion

    //region Network
    @Override
    protected void read(MCDataInput packet, int key) {
        switch (key) {
            case KEY_SIGNAL:
                signal = packet.readByte();
                if (Configurator.staticWires) tile().markRender();
                break;
            default:
                super.read(packet, key);
        }
    }

    protected void sendSignalUpdate() {
        sendUpdate(KEY_SIGNAL, p -> p.writeByte(signal));
    }
    //endregion

    protected int redstoneSignalLevel() {
        return RedstonePropagator.canRedwiresProvidePower() ? ((signal & 0xFF) + 16) / 17 : 0;
    }

    //region IFaceRedstonePart overrides
    @Override
    public int weakPowerLevel(int side) {
        // If side is towards a rotation
        if ((side & 6) != (getSide() & 6)) {
            int r = Rotation.rotationTo(getSide(), side);
            if (maskConnectsInside(r))
                return 0;
        }
        return redstoneSignalLevel();
    }

    @Override
    public boolean canConnectRedstone(int side) {
        return RedstonePropagator.canConnectRedwires();
    }

    @Override
    public int getFace() {
        return getSide();
    }
    //endregion

    //region IConnectableFacePart overrides
    @Override
    public void maskChangeEvent(boolean internalChange, boolean externalChange) {
        super.maskChangeEvent(internalChange, externalChange);
        RedstonePropagator.logCalculation();
        RedstonePropagator.propagateTo(this, internalChange || externalChange ? FORCE : RISING);
    }
    //endregion

    //region IConnectable overrides
    @Override
    public boolean discoverOpen(int r) {
        int absDir = IConnectableFacePart.absoluteDir(this, r);
        RedstoneTile tile = (RedstoneTile) this.tile(); // IRedstoneTile is mixed in
        return (tile.openConnections(absDir) & 1 << Rotation.rotationTo(absDir & 6, getSide())) != 0;
    }

    @Override
    public boolean discoverStraightOverride(int absDir) {
        boolean prevCanConnectRW = RedstonePropagator.canConnectRedwires();
        RedstonePropagator.setCanConnectRedwires(true);
        boolean discovered = (RedstoneInteractions.otherConnectionMask(level(), pos(), absDir, false) &
                RedstoneInteractions.connectionMask(this, absDir)) != 0;
        RedstonePropagator.setCanConnectRedwires(prevCanConnectRW);
        return discovered;
    }

    @Override
    public boolean discoverInternalOverride(int r) {
        MultiPart part = tile().getSlottedPart(IConnectableFacePart.absoluteDir(this, r));
        if (part instanceof FaceRedstonePart) {
            return ((FaceRedstonePart) part).canConnectRedstone(getSide());
        }
        return false;
    }

    @Override
    public boolean canConnectPart(IConnectable part, int dir) {
        return part instanceof IRedwireEmitter || part instanceof RedstonePart;
    }
    //endregion

    //region IRedstonePropagationPart overrides
    @Override
    public void onSignalUpdate() {
        sendSignalUpdate();
    }

    @Override
    public int getSignal() {
        return signal & 0xFF;
    }

    @Override
    public void setSignal(int signal) {
        this.signal = (byte) signal;
    }

    @Override
    public int calculateSignal() {
        RedstonePropagator.setDustProvidesPower(false);
        RedstonePropagator.setRedwiresProvidePower(false);

        int signal = 0;

        for (int r = 0; r < 4; r++) {
            int s = 0;
            if (maskConnectsCorner(r)) {
                FaceLookup lookup = FaceLookup.lookupCorner(level(), pos(), getSide(), r);
                s = resolveSignal(lookup);

            } else if (maskConnectsStraight(r)) {
                FaceLookup lookup = FaceLookup.lookupStraight(level(), pos(), getSide(), r);
                s = resolveSignal(lookup);
                if (s <= 0) {
                    s = RedstoneFaceLookup.resolveVanillaSignal(lookup, this, true, true);
                }

            } else if (maskConnectsInside(r)) {
                FaceLookup lookup = FaceLookup.lookupInsideFace(level(), pos(), getSide(), r);
                s = resolveSignal(lookup);
            }

            signal = Math.max(s, signal);
        }

        if (powerUnderside()) {
            Direction face = Direction.values()[getSide()];
            int s = level().getSignal(pos().relative(face), face) * 17;
            signal = Math.max(s, signal);
        }

        if (maskConnectsCenter()) {
            FaceLookup lookup = FaceLookup.lookupInsideCenter(level(), pos(), getSide());
            int s = resolveSignal(lookup);
            signal = Math.max(s, signal);
        }

        RedstonePropagator.setDustProvidesPower(true);
        RedstonePropagator.setRedwiresProvidePower(true);

        return signal;
    }
    //endregion

    //region IRedwirePart overrides
    @Override
    public int getRedwireSignal(int dir) {
        return getSignal();
    }

    @Override
    public boolean diminishOnSide(int side) {
        return true;
    }
    //endregion

    protected int resolveSignal(FaceLookup lookup) {
        return RedstoneFaceLookup.resolveSignal(lookup, true);
    }

    protected abstract boolean powerUnderside();
}
