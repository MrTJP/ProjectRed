package mrtjp.projectred.transmission.part;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.vec.Rotation;
import codechicken.multipart.api.RedstoneInteractions;
import codechicken.multipart.api.part.TMultiPart;
import codechicken.multipart.api.part.redstone.IFaceRedstonePart;
import codechicken.multipart.api.part.redstone.IRedstonePart;
import codechicken.multipart.trait.extern.IRedstoneTile;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.RedstonePropagator;
import mrtjp.projectred.core.part.IPropagationFacePart;
import mrtjp.projectred.core.part.IRedstonePropagationPart;
import mrtjp.projectred.core.FaceLookup;
import mrtjp.projectred.core.part.IRedwireEmitter;
import mrtjp.projectred.core.part.IRedwirePart;
import mrtjp.projectred.transmission.WireType;
import net.minecraft.block.Blocks;
import net.minecraft.block.RedstoneWireBlock;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.Direction;
import net.minecraft.util.math.BlockPos;

import static mrtjp.projectred.core.RedstonePropagator.FORCE;
import static mrtjp.projectred.core.RedstonePropagator.RISING;

public abstract class RedwirePart extends BaseFaceWirePart implements IRedstonePropagationPart, IPropagationFacePart, IFaceRedstonePart, IRedwirePart {

    private static final int KEY_SIGNAL = 10;

    private byte signal = 0;

    public RedwirePart(WireType wireType) {
        super(wireType);
    }

    @Override
    public void save(CompoundNBT tag) {
        super.save(tag);
        tag.putByte("signal", signal);
    }

    @Override
    public void load(CompoundNBT tag) {
        super.load(tag);
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

    @Override
    protected void read(MCDataInput packet, int key) {
        switch (key) {
            case KEY_SIGNAL:
                signal = packet.readByte();
                if (Configurator.staticWires()) tile().markRender();
                break;
            default:
                super.read(packet, key);
        }
    }

    protected void sendSignalUpdate() {
        sendUpdate(KEY_SIGNAL, p -> p.writeByte(signal));
    }

    @Override
    public void onPartChanged(TMultiPart part) {
        if (!world().isClientSide) {
            RedstonePropagator.logCalculation();
            if (updateOutward()) {
                onMaskChanged();
                RedstonePropagator.propagateTo(this, FORCE);
            } else {
                RedstonePropagator.propagateTo(this, RISING);
            }
        }
    }

    @Override
    public void onNeighborBlockChanged(BlockPos from) {
        if (!world().isClientSide) {
            if (dropIfCantStay()) {
                return;
            }
            RedstonePropagator.logCalculation();
            if (updateExternalConns()) {
                onMaskChanged();
                RedstonePropagator.propagateTo(this, FORCE);
            } else {
                RedstonePropagator.propagateTo(this, RISING);
            }
        }
    }

    @Override
    public void onAdded() {
        super.onAdded();
        if (!world().isClientSide) {
            RedstonePropagator.propagateTo(this, RISING);
        }
    }

    protected int redstoneSignalLevel() {
        return RedstonePropagator.canRedwiresProvidePower() ? ((signal & 0xFF) + 16) / 17 : 0;
    }

    //region IFaceRedstonePart overrides
    @Override
    public int weakPowerLevel(int side) {
        // If side is towards a rotation
        if ((side & 6) != (getSide() & 6)) {
            int r = absoluteRot(side);
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

    //region IConnectable overrides
    @Override
    public boolean discoverOpen(int r) {
        int absDir = absoluteDir(r);
        IRedstoneTile tile = (IRedstoneTile) getTile(); // IRedstoneTile is mixed in
        return (tile.openConnections(absDir) & 1 << Rotation.rotationTo(absDir & 6, getSide())) != 0;
    }

    @Override
    public boolean discoverStraightOverride(int absDir) {
        boolean prevCanConnectRW = RedstonePropagator.canConnectRedwires();
        RedstonePropagator.setCanConnectRedwires(true);
        boolean discovered = (RedstoneInteractions.otherConnectionMask(world(), pos(), absDir, false) &
                RedstoneInteractions.connectionMask(this, absDir)) != 0;
        RedstonePropagator.setCanConnectRedwires(prevCanConnectRW);
        return discovered;
    }

    @Override
    public boolean discoverInternalOverride(int r) {
        TMultiPart part = tile().getSlottedPart(absoluteDir(r));
        if (part instanceof IFaceRedstonePart) {
            return ((IFaceRedstonePart) part).canConnectRedstone(getSide());
        }
        return false;
    }

    @Override
    public boolean canConnectPart(IConnectable part, int dir) {
        return part instanceof IRedwireEmitter || part instanceof IRedstonePart;
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
            if (maskConnectsCorner(r)) {
                signal = Math.max(calcCornerSignal(r), signal);
            } else if (maskConnectsStraight(r)) {
                signal = Math.max(calcStraightSignal(r), signal);
            } else if (maskConnectsInside(r)) {
                signal = Math.max(calcInsideSignal(r), signal);
            }
        }

        if (powerUnderside()) {
            signal = Math.max(calcUndersideSignal(), signal);
        }

        if (maskConnectsCenter()) {
            signal = Math.max(calcCenterSignal(), signal);
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

    protected int calcCornerSignal(int r) {
        FaceLookup lookup = FaceLookup.lookupCorner(world(), pos(), getSide(), r);
        return resolveSignal(lookup);
    }

    protected int calcStraightSignal(int r) {
        FaceLookup lookup = FaceLookup.lookupStraight(world(), pos(), getSide(), r);
        int signal = resolveSignal(lookup);
        if (signal > 0) {
            return signal;
        }
        return getVanillaSignal(r, true, true);
    }

    protected int calcInsideSignal(int r) {
        FaceLookup lookup = FaceLookup.lookupInsideFace(world(), pos(), getSide(), r);
        return resolveSignal(lookup);
    }

    protected int calcUndersideSignal() {
        Direction face = Direction.values()[getSide()];
        return world().getSignal(pos().relative(face), face) * 17;
    }

    protected int calcCenterSignal() {
        FaceLookup lookup = FaceLookup.lookupInsideCenter(world(), pos(), getSide());
        return resolveSignal(lookup);
    }

    protected int resolveSignal(FaceLookup lookup) {

        // Part signal resolution
        if (lookup.part instanceof IRedwirePart) {
            IRedwirePart redwirePart = (IRedwirePart) lookup.part;
            if (redwirePart.diminishOnSide(lookup.otherRotation)) {
                return redwirePart.getRedwireSignal(lookup.otherRotation) - 1;
            }
        }

        if (lookup.part instanceof IRedwireEmitter) {
            return ((IRedwireEmitter) lookup.part).getRedwireSignal(lookup.otherRotation);
        }

        if (lookup.part instanceof IFaceRedstonePart) {
            IFaceRedstonePart faceRsPart = (IFaceRedstonePart) lookup.part;
            int s = Rotation.rotateSide(lookup.otherSide, lookup.otherRotation);
            return Math.max(faceRsPart.strongPowerLevel(s), faceRsPart.weakPowerLevel(s)) * 17;
        }

        return 0;
    }

    protected int getVanillaSignal(int r, boolean strong, boolean limitDust) {
        FaceLookup lookup = FaceLookup.lookupStraight(world(), pos(), getSide(), r);
        int signal = 0;

        // Dust signal
        if (lookup.block == Blocks.REDSTONE_WIRE) {
            signal = Math.max(lookup.state.getValue(RedstoneWireBlock.POWER) - 1, 0);
            if (limitDust) {
                return signal;
            }
        }

        // Strong signal
        int dir = absoluteDir(r);
        signal = RedstoneInteractions.getPowerTo(this, dir) * 17;
        if (signal > 0 && strong) {
            return signal;
        }

        // Weak signal
        if (lookup.state.isRedstoneConductor(world(), lookup.otherPos)) {
            signal = world().getBestNeighborSignal(lookup.otherPos) * 17;
        }

        return signal;
    }

    protected abstract boolean powerUnderside();
}
