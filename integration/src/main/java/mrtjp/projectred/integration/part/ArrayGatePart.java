package mrtjp.projectred.integration.part;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.raytracer.VoxelShapeCache;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.api.RedstoneInteractions;
import codechicken.multipart.api.part.TMultiPart;
import codechicken.multipart.api.part.redstone.IFaceRedstonePart;
import codechicken.multipart.block.BlockMultiPart;
import com.google.common.collect.ImmutableSet;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.FaceLookup;
import mrtjp.projectred.core.RedstonePropagator;
import mrtjp.projectred.core.part.*;
import mrtjp.projectred.integration.GateType;
import net.minecraft.block.Blocks;
import net.minecraft.block.RedstoneWireBlock;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.Direction;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.shapes.ISelectionContext;
import net.minecraft.util.math.shapes.VoxelShape;

public abstract class ArrayGatePart extends RedstoneGatePart implements IRedwirePart, IPropagationFacePart, IRedstonePropagationPart {

    private int propagationMask = 0xF;

    public ArrayGatePart(GateType type) {
        super(type);
    }

    //region Propagation
    @Override
    public int getPropagationMask() {
        return propagationMask;
    }

    @Override
    public int getSignal() {
        return getSignal(toInternalMask(propagationMask));
    }

    @Override
    public void setSignal(int signal) {
        setSignal(toInternalMask(propagationMask), signal);
    }

    @Override
    public void updateAndPropagate(IPropagationPart prev, int mode) {
        int rd = sideDiff(prev);
        int uMask = 0;
        for (int r = 0; r < 4; r++) {
            if ((rd & 1 << r) != 0) {
                int pMask = propagationMask(toInternal(r));
                if (pMask > 0 && (pMask & uMask) != pMask) {
                    propagationMask = toAbsoluteMask(pMask);
                    IRedstonePropagationPart.super.updateAndPropagate(prev, mode);
                    uMask |= pMask;
                }
            }
        }

        if (uMask == 0) {
            RedstonePropagator.addNeighborChange(world(), pos());
        }
        propagationMask = 0xF;
    }

    @Override
    public void propagateOther(int mode) {
        int nonConn = ~(getConnMap() | getConnMap() >> 4 | getConnMap() >> 8) & 0xF;
        notifyExternals(nonConn & propagationMask);
    }

    private int sideDiff(IPropagationPart p) {
        if (!(p instanceof TMultiPart)) return 0xF;

        if (!(p instanceof IOrientableFacePart) || ((TMultiPart) p).tile() == null) return 0xF;

        TMultiPart part = (TMultiPart) p;
        IOrientableFacePart facePart = (IOrientableFacePart) p;
        BlockPos here = pos();
        BlockPos there = part.pos();

        if (here.equals(there) && (getSide() & 6) != (facePart.getSide() & 6)) return 1 << Rotation.rotationTo(getSide(), facePart.getSide());

        // Part around corner and towards our face. Bring up to our face's plane
        if (getSide() != facePart.getSide()) there = there.relative(Direction.values()[getSide() ^ 1]);

        BlockPos diff = there.subtract(here);

        if (diff.getX() == 0 && diff.getY() == 1 && diff.getZ() == 0) return 1 << Rotation.rotationTo(getSide(), 0);
        if (diff.getX() == 0 && diff.getY() == -1 && diff.getZ() == 0) return 1 << Rotation.rotationTo(getSide(), 1);
        if (diff.getX() == 0 && diff.getY() == 0 && diff.getZ() == 1) return 1 << Rotation.rotationTo(getSide(), 2);
        if (diff.getX() == 0 && diff.getY() == 0 && diff.getZ() == -1) return 1 << Rotation.rotationTo(getSide(), 3);
        if (diff.getX() == 1 && diff.getY() == 0 && diff.getZ() == 0) return 1 << Rotation.rotationTo(getSide(), 4);
        if (diff.getX() == -1 && diff.getY() == 0 && diff.getZ() == 0) return 1 << Rotation.rotationTo(getSide(), 5);

        throw new RuntimeException("Propagating to distant part from " + here + " to " + there + "!?");
    }

    //endregion

    //region Redwire signal recalculation
    @Override
    public int calculateSignal() {
        int ipmask = toInternalMask(propagationMask);
        if (overrideSignal(ipmask)) {
            return calculateSignal(ipmask);
        }

        RedstonePropagator.setDustProvidesPower(false);
        RedstonePropagator.setRedwiresProvidePower(false);

        int signal = 0;
        for (int r = 0; r < 4; r++) {
            if ((propagationMask & 1 << r) == 0) { continue; }

            if (maskConnectsCorner(r)) {
                signal = Math.max(calcCornerSignal(r), signal);
            } else if (maskConnectsStraight(r)) {
                signal = Math.max(calcStraightSignal(r), signal);
            } else if (maskConnectsInside(r)) {
                signal = Math.max(calcInsideSignal(r), signal);
            } else {
                signal = Math.max(getVanillaSignal(r, true, true), signal);
            }
        }

        RedstonePropagator.setDustProvidesPower(true);
        RedstonePropagator.setRedwiresProvidePower(true);
        return signal;
    }

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

        // Shouldn't matter, no space for a face part inside
        if (lookup.part instanceof IFaceRedstonePart) {
            IFaceRedstonePart faceRsPart = (IFaceRedstonePart) lookup.part;
            int s = Rotation.rotateSide(lookup.otherSide, lookup.otherRotation);
            return Math.max(faceRsPart.strongPowerLevel(s), faceRsPart.weakPowerLevel(s)) * 17;
        }

        return super.resolveSignal(lookup);
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

    @Override
    public void onSignalUpdate() {
        tile().setChanged();
        super.onChange();
    }
    //endregion

    //region Redwire signal emission
    @Override
    public int getRedwireSignal(int r) {
        int ir = toInternal(r);
        int pmask = propagationMask(ir);
        return pmask != 0 ? getSignal(pmask) : getOutput(ir) * 17;
    }

    @Override
    public boolean diminishOnSide(int r) {
        return (redwireMask(shape()) & 1 << toInternal(r)) != 0;
    }

    @Override
    public boolean canConnectRedstone(int side) {
        if (super.canConnectRedstone(side)) return true;
        if ((side & 6) == (getSide() & 6)) return false;
        return canConnectRedwire(toInternal(absoluteRot(side)));
    }

    protected int rsLevel(int i) {
        return RedstonePropagator.canRedwiresProvidePower() ? (i + 16) / 17 : 0;
    }

    @Override
    public int weakPowerLevel(int side) {
        if ((side & 6) == (getSide() & 6)) return 0;
        int ir = toInternal(absoluteRot(side));
        if ((redwireMask(shape()) & 1 << ir) != 0) {
            return rsLevel(getSignal(propagationMask(ir)));
        }
        return super.weakPowerLevel(side);
    }
    //endregion

    //region Multipart properties
    @Override
    public void preparePlacement(PlayerEntity player, BlockPos pos, int side) {
        super.preparePlacement(player, pos, side);
        if (canCross()) {
            // Note: tile() is not available yet, must access from player.level
            TMultiPart tpart = BlockMultiPart.getPart(player.level, pos, getSide()^1);
            if (tpart instanceof ArrayGatePart) {
                ArrayGatePart part = (ArrayGatePart) tpart;
                if (part.getGateType() == getGateType() && (part.getRotation() & 1) == (getRotation() & 1)) {
                    setRotation((getRotation() + 1) % 4);
                }
            }
        }
    }

    @Override
    public boolean occlusionTest(TMultiPart npart) {
        if (npart instanceof ArrayGatePart) {
            ArrayGatePart part = (ArrayGatePart) npart;
            if (part.getGateType() == getGateType()
                    && part.getSide() == (getSide() ^ 1)
                    && (part.getRotation() & 1) != (getRotation() & 1)) { return true; }
        }
        return super.occlusionTest(npart);
    }

    @Override
    protected void rotate() {
        int r = getRotation();
        setRotation((r + 1) % 4);
        boolean b = tile().canReplacePart(this, this);
        setRotation(r);
        if (b) super.rotate();
    }

    protected boolean canCross() {
        return false;
    }
    //endregion

    //region Connections
    @Override
    protected boolean gateLogicCanConnectTo(IConnectable part, int r) {
        if (part instanceof IRedwirePart) {
            if (canConnectRedwire(r)) return true;
        }
        return super.gateLogicCanConnectTo(part, r);
    }
    //endregion

    //region Gate logic
    @Override
    protected void onChange() {
        super.onChange();
        RedstonePropagator.propagateTo(this, RedstonePropagator.RISING);
    }

    protected boolean canConnectRedwire(int r) {
        return (redwireMask(r) & 1 << r) != 0;
    }

    protected abstract int redwireMask(int shape);

    protected abstract int propagationMask(int r);

    protected abstract int getSignal(int mask);

    protected abstract void setSignal(int mask, int signal);

    protected boolean overrideSignal(int mask) {
        return false;
    }

    protected int calculateSignal(int mask) {
        return 0;
    }
    //endregion

    public interface IGateWireRenderConnect {

        int renderConnectMask();

        double getHeight(int r);

        static int getConnsAtHeight(GatePart gate, double h) {
            int conn = 0;
            for (int r = 0; r < 4; r++) {
                if (getConnHeight(gate, r) == h) {
                    conn |= 1 << r;
                }
            }
            return gate.toInternalMask(conn);
        }

        static double getConnHeight(GatePart gate, int r) {
            IConnectable part = gate.getStraight(r);
            if (part instanceof IGateWireRenderConnect && part instanceof GatePart) {
                GatePart gPart = (GatePart) part;
                IGateWireRenderConnect gConn = (IGateWireRenderConnect) part;
                int ir = gPart.toInternal(gate.rotFromStraight(r));
                if ((gConn.renderConnectMask() & 1 << ir) != 0) return gConn.getHeight(ir);
            }
            return -1;
        }
    }

    public static abstract class ArrayGatePartCrossing extends ArrayGatePart implements IGateWireRenderConnect {

        private static final Cuboid6[][] oBoxes = new Cuboid6[6][2];
        private static final Cuboid6[] cBoxes = new Cuboid6[6];
        private static final VoxelShape[] oShapes = new VoxelShape[6];
        private static final VoxelShape[] cShapes = new VoxelShape[6];

        static {
            for (int s = 0; s < 6; s++) {
                Cuboid6 occlusion1 = new Cuboid6(1 / 8D, 0, 0, 7 / 8D, 6 / 8D, 1);
                Cuboid6 occlusion2 = new Cuboid6(0, 0, 1 / 8D, 1, 6 / 8D, 7 / 8D);
                Cuboid6 collision = new Cuboid6(0, 0, 0, 1, 6 / 8D, 1);

                Transformation t = Rotation.sideRotations[s].at(Vector3.CENTER);
                oBoxes[s][0] = occlusion1.apply(t);
                oBoxes[s][1] = occlusion2.apply(t);
                cBoxes[s] = collision.apply(t);

                ImmutableSet.Builder<VoxelShape> builder = ImmutableSet.builder();
                builder.add(VoxelShapeCache.getShape(oBoxes[s][0]));
                builder.add(VoxelShapeCache.getShape(oBoxes[s][1]));
                oShapes[s] = VoxelShapeCache.merge(builder.build());
                cShapes[s] = VoxelShapeCache.getShape(cBoxes[s]);
            }
        }

        private static final int KEY_SIGNAL = 20;

        private byte signal1 = 0;
        private byte signal2 = 0;

        public ArrayGatePartCrossing(GateType type) {
            super(type);
        }

        //region save/load
        @Override
        public void save(CompoundNBT tag) {
            super.save(tag);
            tag.putByte("s1", signal1);
            tag.putByte("s2", signal2);
        }

        @Override
        public void load(CompoundNBT tag) {
            super.load(tag);
            signal1 = tag.getByte("s1");
            signal2 = tag.getByte("s2");
        }

        @Override
        public void writeDesc(MCDataOutput packet) {
            super.writeDesc(packet);
            packet.writeByte(signal1);
            packet.writeByte(signal2);
        }

        @Override
        public void readDesc(MCDataInput packet) {
            super.readDesc(packet);
            signal1 = packet.readByte();
            signal2 = packet.readByte();
        }
        //endregion

        //region Packets
        @Override
        protected void read(MCDataInput packet, int key) {
            switch (key) {
                case KEY_SIGNAL:
                    signal1 = packet.readByte();
                    signal2 = packet.readByte();
                    if (Configurator.staticGates) tile().markRender();
                    break;
                default:
                    super.read(packet, key);
            }
        }

        protected void sendSignalUpdate() {
            sendUpdate(KEY_SIGNAL, w -> w.writeByte(signal1).writeByte(signal2));
        }
        //endregion

        //region Multipart properties
        @Override
        public VoxelShape getCollisionShape(ISelectionContext context) {
            return cShapes[getSide()];
        }

        @Override
        public VoxelShape getShape(ISelectionContext context) {
            return getCollisionShape(context);
        }

        @Override
        public VoxelShape getOcclusionShape() {
            return oShapes[getSide()];
        }
        //endregion

        //region Render data
        @Override
        public byte bottomSignal() {
            return signal1;
        }

        @Override
        public byte topSignal() {
            return signal2;
        }

        @Override
        public int topSignalConnMask() {
            return IGateWireRenderConnect.getConnsAtHeight(this, 10.0D);
        }
        //endregion

        //region Gate logic
        @Override
        protected int redwireMask(int shape) {
            return 0xF;
        }

        @Override
        protected int propagationMask(int r) {
            return r % 2 == 0 ? 0x5 : 0xA;
        }

        @Override
        protected int outputMask(int shape) {
            return 0xF;
        }

        @Override
        protected int inputMask(int shape) {
            return 0xF;
        }

        @Override
        public int renderConnectMask() {
            return 0xA;
        }

        @Override
        public double getHeight(int r) {
            return 10.0D;
        }

        @Override
        protected int getSignal(int mask) {
            return (mask == 0x5 ? signal1 : signal2) & 0xFF;
        }

        @Override
        protected void setSignal(int mask, int signal) {
            if (mask == 0x5) {
                signal1 = (byte) signal;
            } else {
                signal2 = (byte) signal;
            }
        }

        @Override
        public void onSignalUpdate() {
            super.onSignalUpdate();
            sendSignalUpdate();
        }

        @Override
        protected void gateLogicOnChange() {
            boolean oldSignal = (state() & 1) != 0;
            boolean newSignal = signal1 != 0;

            if (oldSignal != newSignal) {
                setState(state() & 2 | (newSignal ? 1 : 0));
                onInputChange();
                scheduleTick(2);
            }
        }

        @Override
        protected void gateLogicOnScheduledTick() {
            boolean input = (state() & 1) != 0;
            boolean oldOutput = (state() & 2) != 0;
            boolean newOutput = !input;

            if (oldOutput != newOutput) {
                setState(state() & 1 | (newOutput ? 2 : 0));
                onOutputChange(0);
                onChange();
            }
        }

        @Override
        public int calculateSignal(int mask) {
            return 0xFF;
        }

        @Override
        protected boolean overrideSignal(int mask) {
            return mask == 0xA && powerUp();
        }

        protected boolean powerUp() {
            return false;
        }
        //endregion
    }

    public static class NullCell extends ArrayGatePartCrossing {

        public NullCell(GateType type) {
            super(type);
        }

        @Override
        protected boolean canCross() {
            return true;
        }

        @Override
        public int getLightValue() {
            // No redstone torch in model
            return 0;
        }
    }

    public static class InvertCell extends ArrayGatePartCrossing {

        public InvertCell(GateType type) {
            super(type);
        }

        @Override
        protected boolean powerUp() {
            return (state() & 2) != 0;
        }
    }

    public static class BufferCell extends ArrayGatePartCrossing {

        public BufferCell(GateType type) {
            super(type);
        }

        @Override
        protected boolean powerUp() {
            return (state() & 2) == 0;
        }
    }

    public static abstract class TopWireArrayGate extends ArrayGatePart implements IGateWireRenderConnect {

        private static final int KEY_SIGNAL = 20;

        protected byte signal = 0;

        public TopWireArrayGate(GateType type) {
            super(type);
        }

        //region save/load
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
        //endregion

        //region Packets
        @Override
        protected void read(MCDataInput packet, int key) {
            switch (key) {
                case KEY_SIGNAL:
                    signal = packet.readByte();
                    if (Configurator.staticGates) tile().markRender();
                    break;
                default:
                    super.read(packet, key);
            }
        }

        protected void sendSignalUpdate() {
            sendUpdate(KEY_SIGNAL, w -> w.writeByte(signal));
        }
        //endregion

        //region Multipart properties
        @Override
        public VoxelShape getCollisionShape(ISelectionContext context) {
            return ArrayGatePartCrossing.cShapes[getSide()];
        }

        @Override
        public VoxelShape getShape(ISelectionContext context) {
            return getCollisionShape(context);
        }

        @Override
        public VoxelShape getOcclusionShape() {
            return ArrayGatePartCrossing.oShapes[getSide()];
        }
        //endregion

        @Override
        public void onSignalUpdate() {
            super.onSignalUpdate();
            sendSignalUpdate();
        }

        //region Render data
        @Override
        public byte topSignal() {
            return signal;
        }

        @Override
        public int topSignalConnMask() {
            return IGateWireRenderConnect.getConnsAtHeight(this, 10.0D);
        }

        @Override
        public int renderConnectMask() {
            return 0xA;
        }

        @Override
        public double getHeight(int r) {
            return 10.0D;
        }
        //endregion

        //region Gate logic
        @Override
        protected int redwireMask(int shape) {
            return 0xA;
        }

        @Override
        protected int propagationMask(int r) {
            return r % 2 == 1 ? 0xA : 0;
        }

        @Override
        protected int getSignal(int mask) {
            return mask == 0xA ? signal & 0xFF : 0;
        }

        @Override
        protected void setSignal(int mask, int signal) {
            if (mask == 0xA) {
                this.signal = (byte) signal;
            }
        }
        //endregion
    }

    public static class SimpleTopWireArrayGate extends TopWireArrayGate {

        public SimpleTopWireArrayGate(GateType type) {
            super(type);
        }

        //region Gate logic
        @Override
        protected void gateLogicOnChange() {
            int iMask = inputMask(shape());
            int oMask = outputMask(shape());
            int fMask = feedbackMask(shape());
            int oldInput = getState() & 0xF;
            int newInput = getInput(iMask | fMask);
            if (oldInput != newInput) {
                setState(getState() & 0xF0 | newInput);
                onInputChange();
            }

            int newOutput = calcOutput(state() & iMask) & oMask;
            if (newOutput != (state() >> 4)) scheduleTick(getDelay(shape()));
        }

        @Override
        protected void gateLogicOnScheduledTick() {
            int iMask = inputMask(shape());
            int oMask = outputMask(shape());
            int oldOutput = state() >> 4;
            int newOutput = calcOutput(state() & iMask) & oMask;
            if (oldOutput != newOutput) {
                setState(state() & 0xF | newOutput << 4);
                onOutputChange(oMask);
            }
            gateLogicOnChange();
        }

        @Override
        protected void gateLogicSetup() {
            int iMask = inputMask(shape());
            int oMask = outputMask(shape());
            int output = calcOutput(getInput(iMask)) & oMask;
            if (output != 0) {
                setState(output << 4);
                onOutputChange(output); //use output for change mask because nothing is going low
            }
        }

        int getDelay(int shape) {
            return 2;
        }

        int feedbackMask(int shape) {
            return 0;
        }

        int calcOutput(int input) {
            return 0;
        }
        //endregion

    }

    public static class ANDCell extends SimpleTopWireArrayGate {

        public ANDCell(GateType type) {
            super(type);
        }

        @Override
        protected int outputMask(int shape) {
            return 1;
        }

        @Override
        protected int inputMask(int shape) {
            return 4;
        }

        @Override
        int calcOutput(int input) {
            return input == 4 && signal != 0 ? 1 : 0;
        }
    }

    public static class TransparentLatchCell extends SimpleTopWireArrayGate {

        public TransparentLatchCell(GateType type) {
            super(type);
        }

        @Override
        protected int outputMask(int shape) {
            return 1;
        }

        @Override
        protected int inputMask(int shape) {
            return 4;
        }

        @Override
        int calcOutput(int input) {
            return signal == 0 ? state() >> 4 : (input & 4) == 0 ? 0 : 1;
        }
    }
}
