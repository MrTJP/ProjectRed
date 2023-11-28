package mrtjp.projectred.transmission.part;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.multipart.api.RedstoneInteractions;
import codechicken.multipart.api.part.MultiPart;
import codechicken.multipart.api.part.redstone.MaskedRedstonePart;
import codechicken.multipart.api.part.redstone.RedstonePart;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.CenterLookup;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.RedstonePropagator;
import mrtjp.projectred.core.part.IPropagationCenterPart;
import mrtjp.projectred.core.part.IRedstonePropagationPart;
import mrtjp.projectred.core.part.IRedwireEmitter;
import mrtjp.projectred.core.part.IRedwirePart;
import mrtjp.projectred.transmission.WireType;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;

import static mrtjp.projectred.core.RedstonePropagator.FORCE;
import static mrtjp.projectred.core.RedstonePropagator.RISING;

public abstract class FramedRedwirePart extends BaseCenterWirePart implements IRedstonePropagationPart, IPropagationCenterPart, MaskedRedstonePart, IRedwirePart {

    private static final int KEY_SIGNAL = 10;

    private byte signal = 0;

    public FramedRedwirePart(WireType wireType) {
        super(wireType);
    }

    @Override
    public void save(CompoundTag tag) {
        super.save(tag);
        tag.putByte("signal", signal);
    }

    @Override
    public void load(CompoundTag tag) {
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
                if (Configurator.staticWires) tile().markRender();
                break;
            default:
                super.read(packet, key);
        }
    }

    protected void sendSignalUpdate() {
        sendUpdate(KEY_SIGNAL, p -> p.writeByte(signal));
    }

    @Override
    public void onPartChanged(MultiPart part) {
        if (!level().isClientSide) {
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
        if (!level().isClientSide) {
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
        if (!level().isClientSide) {
            RedstonePropagator.propagateTo(this, RISING);
        }
    }

    protected int redstoneSignalLevel() {
        return RedstonePropagator.canRedwiresProvidePower() ? ((signal & 0xFF) + 16) / 17 : 0;
    }

    //region IMaskedRestonePart overrides
    @Override
    public int strongPowerLevel(int side) {
        return 0;
    }

    @Override
    public int weakPowerLevel(int side) {
        return redstoneSignalLevel();
    }

    @Override
    public boolean canConnectRedstone(int side) {
        return true;
    }

    @Override
    public int getConnectionMask(int side) {
        return 0x10;
    }
    //endregion

    //region IConnectable overrides
    @Override
    public boolean canConnectPart(IConnectable part, int dir) {
        return part instanceof IRedwirePart;
    }

    @Override
    public boolean discoverStraightOverride(int s) {
        boolean prevCanConnectRW = RedstonePropagator.canConnectRedwires();
        RedstonePropagator.setCanConnectRedwires(false);
        boolean discovered = (RedstoneInteractions.otherConnectionMask(level(), pos(), s, false) &
                RedstoneInteractions.connectionMask(this, s)) != 0;
        RedstonePropagator.setCanConnectRedwires(prevCanConnectRW);
        return discovered;
    }

    @Override
    public boolean discoverInternalOverride(int s) {
        MultiPart part = tile().getSlottedPart(s);
        if (part instanceof RedstonePart) {
            return ((RedstonePart) part).canConnectRedstone(s ^ 1);
        }
        return false;
    }
    //endregion

    //region IRedstonePropagationPart overrides
    @Override
    public int getSignal() {
        return signal & 0xFF;
    }

    @Override
    public void setSignal(int signal) {
        this.signal = (byte) signal;
    }

    @Override
    public void onSignalUpdate() {
        sendSignalUpdate();
    }

    @Override
    public int calculateSignal() {
        RedstonePropagator.setDustProvidesPower(false);
        RedstonePropagator.setRedwiresProvidePower(false);

        int signal = 0;
        for (int s = 0; s < 6; s++) {
            if (maskConnectsIn(s)) {
                signal = Math.max(signal, calculateInternalSignal(s));
            } else if (maskConnectsOut(s)) {
                signal = Math.max(signal, calculateStraightSignal(s));
            }
        }

        RedstonePropagator.setDustProvidesPower(true);
        RedstonePropagator.setRedwiresProvidePower(true);
        return signal;
    }

    @Override
    public void propagateOther(int mode) {
        for (int s = 0; s < 6; s++) {
            if (!maskConnects(s)) {
                RedstonePropagator.addNeighborChange(level(), pos(), posOfStraight(s));
            }
        }
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

    protected int calculateStraightSignal(int s) {
        CenterLookup lookup = CenterLookup.lookupStraightCenter(level(), pos(), s);
        if (lookup.part != null) {
            return resolveSignal(lookup);
        }

        // If no part, get generic strong signal
        return RedstoneInteractions.getPowerTo(this, s) * 17;
    }

    protected int calculateInternalSignal(int s) {
        CenterLookup lookup = CenterLookup.lookupInsideFace(level(), pos(), s);
        int signal = resolveSignal(lookup);
        if (signal > 0) {
            return signal;
        }

        if (lookup.part instanceof RedstonePart) {
            RedstonePart rw = (RedstonePart) lookup.part;
            return Math.max(rw.strongPowerLevel(lookup.otherDirection), rw.weakPowerLevel(lookup.otherDirection));
        }

        return 0;
    }

    protected int resolveSignal(CenterLookup lookup) {
        if (lookup.part instanceof IRedwirePart) {
            IRedwirePart rw = (IRedwirePart) lookup.part;

            int signal = rw.getRedwireSignal(lookup.otherDirection);
            if (rw.diminishOnSide(lookup.otherDirection)) {
                signal--;
            }
            return signal;
        }

        if (lookup.part instanceof IRedwireEmitter) {
            return ((IRedwireEmitter) lookup.part).getRedwireSignal(lookup.otherDirection);
        }

        return 0;
    }
}
