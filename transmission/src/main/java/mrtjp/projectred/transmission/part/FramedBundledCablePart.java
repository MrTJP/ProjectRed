package mrtjp.projectred.transmission.part;

import mrtjp.projectred.api.IBundledEmitter;
import mrtjp.projectred.api.IBundledTile;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.api.IMaskedBundledTile;
import mrtjp.projectred.core.BundledSignalsLib;
import mrtjp.projectred.core.CenterLookup;
import mrtjp.projectred.core.RedstonePropagator;
import mrtjp.projectred.core.part.IPropagationCenterPart;
import mrtjp.projectred.transmission.WireType;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;

import javax.annotation.Nullable;
import java.util.Arrays;
import java.util.Objects;

import static mrtjp.projectred.core.BundledSignalsLib.raiseSignal;
import static mrtjp.projectred.core.RedstonePropagator.FORCE;
import static mrtjp.projectred.core.RedstonePropagator.RISING;

public class FramedBundledCablePart extends BaseCenterWirePart implements IBundledCablePart, IBundledPropagationPart, IPropagationCenterPart {

    private final byte[] signal = new byte[16];
    private final byte[] tmpSignal = new byte[16]; // Used when re-calculating new signal

    private int colourPropagationMask = 0xFFFF;

    public FramedBundledCablePart(WireType wireType) {
        super(wireType);
    }

    //region Trait variables
    @Override
    public byte[] getSignal() {
        return signal;
    }

    @Override
    public void setSignal(@Nullable byte[] signal) {
        if (signal == null) {
            Arrays.fill(this.signal, (byte) 0);
        } else {
            System.arraycopy(signal, 0, this.signal, 0, 16);
        }
    }

    @Override
    public int getColorMask() {
        return colourPropagationMask;
    }

    @Override
    public void setColorMask(int mask) {
        colourPropagationMask = mask;
    }
    //endregion

    //region Save/Load
    @Override
    public void save(CompoundTag tag) {
        super.save(tag);
        tag.putByteArray("signal", signal);
    }

    @Override
    public void load(CompoundTag tag) {
        super.load(tag);
        setSignal(tag.getByteArray("signal"));
    }
    //endregion

    //region IConnectableCenterPart overrides
    @Override
    public void maskChangeEvent(boolean internalChange, boolean externalChange) {
        super.maskChangeEvent(internalChange, externalChange);
        RedstonePropagator.logCalculation();
        RedstonePropagator.propagateTo(this, internalChange || externalChange ? FORCE : RISING);
    }
    //endregion

    //region IConnectable overrides
    @Override
    public boolean canConnectPart(IConnectable part, int dir) {
        if (part instanceof IBundledCablePart) {
            int b1 = ((IBundledCablePart) part).getBundledColour();
            int b2 = getBundledColour();

            // Colours have to match, or one of them has to be neutral
            return b1 == -1 || b2 == -1 || b1 == b2;
        } else if (part instanceof IInsulatedRedwirePart) {
            return true;
        } else if (part instanceof IBundledEmitter) {
            return true;
        }
        return false;
    }

    @Override
    public boolean discoverStraightOverride(int absDir) {
        CenterLookup lookup = CenterLookup.lookupStraightCenter(level(), pos(), absDir);

        if (lookup.tile instanceof IMaskedBundledTile b) {
            return b.canConnectBundled(absDir^1) && (b.getConnectionMask(absDir^1) & 0x10) != 0;
        }

        if (lookup.tile instanceof IBundledTile b) {
            return b.canConnectBundled(absDir^1);
        }

        return BundledSignalsLib.canConnectBundledViaInteraction(level(), lookup.otherPos, Direction.values()[absDir^1]); //TODO add otherDir to lookup
    }
    //endregion

    //region IBundledEmitter and IBundledCablePart overrides
    @Override
    public @Nullable byte[] getBundledSignal(int dir) {
        return maskConnects(dir) ? signal : null;
    }

    @Override
    public void onSignalUpdate() {
    }

    @Override
    public byte[] getBundledSignal() {
        return signal;
    }

    @Override
    public int getBundledColour() {
        return getWireType().getColourIdx();
    }
    //endregion

    //region Signal recalculation
    @Override
    public byte[] calculateSignal() {
        Arrays.fill(tmpSignal, (byte) 0);

        for (int s = 0; s < 6; s++) {

            if (maskConnectsOut(s)) {
                calcCenterToCenterSignal(s);
            } else if (maskConnectsIn(s)) {
                calcCenterToInsideFaceSignal(s);
            }
        }

        return tmpSignal;
    }

    protected void calcCenterToCenterSignal(int s) {
        CenterLookup lookup = CenterLookup.lookupStraightCenter(level(), pos(), s);
        resolveSignal(lookup);
    }

    protected void calcCenterToInsideFaceSignal(int s) {
        CenterLookup lookup = CenterLookup.lookupInsideFace(level(), pos(), s);
        resolveSignal(lookup);
    }

    protected void resolveSignal(CenterLookup lookup) {

        if (lookup.part instanceof IBundledCablePart) {
            byte[] signalIn = ((IBundledCablePart) lookup.part).getBundledSignal();
            for (int i = 0; i < 16; i++) {
                tmpSignal[i] = (byte) Math.max(tmpSignal[i] & 0xFF, (signalIn[i] & 0xFF) - 1);
            }

        } else if (lookup.part instanceof IInsulatedRedwirePart insulatedWire) {
            int c = insulatedWire.getInsulatedColour();
            int signalIn = insulatedWire.getRedwireSignal(lookup.otherDirection);
            tmpSignal[c] = (byte) Math.max(tmpSignal[c] & 0xFF, signalIn - 1);

        } else if (lookup.part instanceof IBundledEmitter) {
            byte[] signalIn = ((IBundledEmitter) lookup.part).getBundledSignal(lookup.otherDirection);
            raiseSignal(tmpSignal, signalIn);

        } else if (lookup.tile instanceof IBundledTile) {
            byte[] signalIn = ((IBundledTile) lookup.tile).getBundledSignal(lookup.otherDirection);
            raiseSignal(tmpSignal, signalIn);

        } else if (lookup.tile != null) {
            byte[] externalSignal = BundledSignalsLib.getBundledSignalViaInteraction(Objects.requireNonNull(lookup.tile.getLevel()), lookup.tile.getBlockPos(), Direction.values()[lookup.otherDirection]);
            raiseSignal(tmpSignal, externalSignal);
        }
    }
    //endregion

    //region Rendering
    @Override
    public boolean useStaticRenderer() {
        // This part does not change appearance based on signal, so no advantage
        // to using the dynamic renderer.
        return true;
    }
    //endregion
}
