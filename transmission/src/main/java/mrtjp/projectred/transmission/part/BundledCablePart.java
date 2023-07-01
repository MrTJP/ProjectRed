package mrtjp.projectred.transmission.part;

import codechicken.lib.vec.Rotation;
import codechicken.multipart.api.part.MultiPart;
import mrtjp.projectred.api.IBundledEmitter;
import mrtjp.projectred.api.IBundledTile;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.api.IMaskedBundledTile;
import mrtjp.projectred.core.BundledSignalsLib;
import mrtjp.projectred.core.FaceLookup;
import mrtjp.projectred.core.RedstonePropagator;
import mrtjp.projectred.core.part.IPropagationFacePart;
import mrtjp.projectred.transmission.WireType;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.level.block.entity.BlockEntity;

import java.util.Arrays;

import static mrtjp.projectred.core.BundledSignalsLib.raiseSignal;
import static mrtjp.projectred.core.RedstonePropagator.FORCE;
import static mrtjp.projectred.core.RedstonePropagator.RISING;

public class BundledCablePart extends BaseFaceWirePart implements IBundledCablePart, IBundledPropagationPart, IPropagationFacePart {

    private final byte[] signal = new byte[16];
    private final byte[] tmpSignal = new byte[16]; // Used when re-calculating new signal

    private int colourPropagationMask = 0xFFFF;

    public BundledCablePart(WireType wireType) {
        super(wireType);
    }

    //region Trait variables
    @Override
    public byte[] getSignal() {
        return signal;
    }

    @Override
    public void setSignal(byte[] signal) {
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

    //region TMultiPart overrides
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
        if (!level().isClientSide) {
            RedstonePropagator.propagateTo(this, RISING);
        }
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
        BlockPos pos = pos().relative(Direction.values()[absDir]);
        BlockEntity tile = level().getBlockEntity(pos);
        if (tile instanceof IMaskedBundledTile) {
            IMaskedBundledTile b = (IMaskedBundledTile) tile;
            int r = Rotation.rotationTo(absDir, getSide());
            return b.canConnectBundled(absDir^1) && (b.getConnectionMask(absDir^1) & 1<< r) != 0;
        }

        if (tile instanceof IBundledTile) {
            return ((IBundledTile) tile).canConnectBundled(absDir^1);
        }

        return BundledSignalsLib.canConnectBundledViaInteraction(level(), pos, Direction.values()[absDir^1]);
    }
    //endregion

    //region IBundledEmitter and IBundledCablePart overrides
    @Override
    public byte[] getBundledSignal(int dir) {
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

        for (int r = 0; r < 4; r++) {
            if (maskConnectsCorner(r)) {
                calcCornerSignal(r);
            } else if (maskConnectsStraight(r)) {
                calcStraightSignal(r);
            } else if (maskConnectsInside(r)) {
                calcInsideSignal(r);
            }
        }

        if (maskConnectsCenter()) {
            calcCenterSignal();
        }

        return tmpSignal;
    }

    protected void calcCornerSignal(int r) {
        FaceLookup lookup = FaceLookup.lookupCorner(level(), pos(), getSide(), r);
        resolveSignal(lookup);
    }

    protected void calcStraightSignal(int r) {
        FaceLookup lookup = FaceLookup.lookupStraight(level(), pos(), getSide(), r);
        resolveSignal(lookup);
    }

    protected void calcInsideSignal(int r) {
        FaceLookup lookup = FaceLookup.lookupInsideFace(level(), pos(), getSide(), r);
        resolveSignal(lookup);
    }

    protected void calcCenterSignal() {
        FaceLookup lookup = FaceLookup.lookupInsideCenter(level(), pos(), getSide());
        resolveSignal(lookup);
    }

    protected void resolveSignal(FaceLookup lookup) {

        if (lookup.part instanceof IBundledCablePart) {
            byte[] signalIn = ((IBundledCablePart) lookup.part).getBundledSignal();
            for (int i = 0; i < 16; i++) {
                tmpSignal[i] = (byte) Math.max(tmpSignal[i] & 0xFF, (signalIn[i] & 0xFF) - 1);
            }

        } else if (lookup.part instanceof IInsulatedRedwirePart) {
            IInsulatedRedwirePart insulatedWire = (IInsulatedRedwirePart) lookup.part;
            int c = insulatedWire.getInsulatedColour();
            int signalIn = insulatedWire.getRedwireSignal(lookup.otherRotation);
            tmpSignal[c] = (byte) Math.max(tmpSignal[c] & 0xFF, signalIn - 1);

        } else if (lookup.part instanceof IBundledEmitter) {
            byte[] signalIn = ((IBundledEmitter) lookup.part).getBundledSignal(lookup.otherRotation);
            raiseSignal(tmpSignal, signalIn);

        } else if (lookup.tile instanceof IBundledTile) {
            byte[] signalIn = ((IBundledTile) lookup.tile).getBundledSignal(Rotation.rotateSide(lookup.otherSide, lookup.otherRotation));
            raiseSignal(tmpSignal, signalIn);

        } else if (lookup.tile != null) {
            byte[] externalSignal = BundledSignalsLib.getBundledSignalViaInteraction(lookup.tile.getLevel(), lookup.tile.getBlockPos(), Direction.values()[Rotation.rotateSide(lookup.otherSide, lookup.otherRotation)]);
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
