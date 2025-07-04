package mrtjp.projectred.integration.part;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.raytracer.IndexedVoxelShape;
import codechicken.lib.raytracer.MultiIndexedVoxelShape;
import codechicken.lib.raytracer.VoxelShapeCache;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Vector3;
import codechicken.microblock.part.face.FaceMicroblockPart;
import codechicken.multipart.util.PartRayTraceResult;
import com.google.common.collect.ImmutableSet;
import mrtjp.projectred.api.*;
import mrtjp.projectred.core.BundledSignalsLib;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.FaceLookup;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.lib.VecLib;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.phys.shapes.CollisionContext;
import net.minecraft.world.phys.shapes.VoxelShape;

import javax.annotation.Nullable;
import java.util.*;

import static mrtjp.projectred.core.BundledSignalsLib.*;
import static mrtjp.projectred.core.part.IOrientableFacePart.flipMaskZ;

public abstract class BundledGatePart extends RedstoneGatePart implements IBundledEmitter {

    public BundledGatePart(GateType type) {
        super(type);
    }

    //region Bundled emitters
    @Override
    public @Nullable byte[] getBundledSignal(int r) {
        int ir = toInternal(r);
        if ((bundledOutputMask(shape()) & 1 << ir) != 0) {
            return getBundledOutput(ir);
        }
        return null;
    }
    //endregion

    //region Signal acquisition
    protected @Nullable byte[] getBundledInput(int r) {
        int ar = toAbsolute(r);
        if (maskConnectsCorner(ar)) {
            return calcCornerSignal(ar);
        } else if (maskConnectsStraight(ar)) {
            return calcStraightSignal(ar);
        } else if (maskConnectsInside(ar)) {
            return calcInsideSignal(ar);
        }
        return null;
    }

    private @Nullable byte[] calcCornerSignal(int r) {
        FaceLookup lookup = FaceLookup.lookupCorner(level(), pos(), getSide(), r);
        return resolveArray(lookup);
    }

    private @Nullable byte[] calcStraightSignal(int r) {
        FaceLookup lookup = FaceLookup.lookupStraight(level(), pos(), getSide(), r);
        return resolveArray(lookup);
    }

    private @Nullable byte[] calcInsideSignal(int r) {
        FaceLookup lookup = FaceLookup.lookupInsideFace(level(), pos(), getSide(), r);
        return resolveArray(lookup);
    }

    protected @Nullable byte[] resolveArray(FaceLookup lookup) {
        if (lookup.part instanceof IBundledEmitter) {
            return ((IBundledEmitter) lookup.part).getBundledSignal(lookup.otherRotation);

        } else if (lookup.tile instanceof IBundledTile) {
            return ((IBundledTile) lookup.tile).getBundledSignal(Rotation.rotateSide(lookup.otherSide, lookup.otherRotation));

        } else if (lookup.tile != null) {
            return BundledSignalsLib.getBundledSignalViaInteraction(Objects.requireNonNull(lookup.tile.getLevel()), lookup.tile.getBlockPos(), Direction.values()[Rotation.rotateSide(lookup.otherSide, lookup.otherRotation)]);
        }
        return null;
    }
    //endregion

    //region Connections
    @Override
    public boolean discoverStraightOverride(int absDir) {
        FaceLookup lookup = FaceLookup.lookupStraight(level(), pos(), getSide(), Rotation.rotationTo(getSide(), absDir));

        if (lookup.tile instanceof IMaskedBundledTile b) {
            int r = Rotation.rotationTo(absDir, getSide());
            return b.canConnectBundled(absDir ^ 1) && (b.getConnectionMask(absDir ^ 1) & 1 << r) != 0;
        }

        if (lookup.tile instanceof IBundledTile b) {
            return b.canConnectBundled(absDir ^ 1);
        }

        if (BundledSignalsLib.canConnectBundledViaInteraction(level(), lookup.otherPos, Direction.values()[absDir ^ 1])) { //TODO add otherDir to lookup
            return true;
        }

        return super.discoverStraightOverride(absDir);
    }
    //endregion

    //region Multipart properties
    @Override
    public int getLightEmission() {
        // Bundled gates don't typically have rs torches
        return 0;
    }
    //endregion

    //region Gate logic
    @Override
    protected boolean gateLogicCanConnectTo(IConnectable part, int r) {
        if (part instanceof IBundledEmitter) {
            return canConnectBundled(r);
        }
        return super.gateLogicCanConnectTo(part, r);
    }

    protected boolean canConnectBundled(int r) {
        return ((bundledInputMask(shape()) | bundledOutputMask(shape())) & 1 << r) != 0;
    }

    protected int bundledOutputMask(int shape) {
        return 0;
    }

    protected int bundledInputMask(int shape) {
        return 0;
    }

    protected @Nullable byte[] getBundledOutput(int r) {
        return null;
    }
    //endregion

    public static class BusTransceiver extends BundledGatePart {

        private static final int KEY_PACKED_IO = 20;

        private @Nullable byte[] input0, output0, input2, output2 = null;
        private int packedOutput = 0;

        public BusTransceiver(GateType type) {
            super(type);
        }

        //region save/load
        @Override
        public void save(CompoundTag tag, HolderLookup.Provider lookupProvider) {
            super.save(tag, lookupProvider);
            saveSignal(tag, "in0", input0);
            saveSignal(tag, "out0", output0);
            saveSignal(tag, "in2", input2);
            saveSignal(tag, "out2", output2);
        }

        @Override
        public void load(CompoundTag tag, HolderLookup.Provider lookupProvider) {
            super.load(tag, lookupProvider);
            input0 = loadSignal(tag, "in0");
            output0 = loadSignal(tag, "out0");
            input2 = loadSignal(tag, "in2");
            output2 = loadSignal(tag, "out2");
        }

        @Override
        public void writeDesc(MCDataOutput packet) {
            super.writeDesc(packet);
            packet.writeInt(packClientData());
        }

        @Override
        public void readDesc(MCDataInput packet) {
            super.readDesc(packet);
            unpackClientData(packet.readInt());
        }
        //endregion

        //region Packets
        @Override
        protected void read(MCDataInput packet, int key) {
            switch (key) {
                case KEY_PACKED_IO:
                    unpackClientData(packet.readInt());
                    if (Configurator.staticGates) tile().markRender();
                    break;
                default:
                    super.read(packet, key);
            }
        }

        protected void sendClientUpdate() {
            sendUpdate(KEY_PACKED_IO, p -> p.writeInt(packClientData()));
        }

        private int packClientData() {
            return packDigital(output0) | packDigital(output2) << 16;
        }

        private void unpackClientData(int packed) {
            packedOutput = packed;
            output0 = unpackDigital(output0, packed & 0xFFFF);
            output2 = unpackDigital(output2, packed >>> 16);
        }
        //endregion

        //region Render data
        @Override
        public short bOutput0() {
            return (short) (packedOutput & 0xFFFF);
        }

        @Override
        public short bOutput2() {
            return (short) (packedOutput >>> 16);
        }
        //endregion

        //region Gate logic

        @Override
        protected int bundledOutputMask(int shape) {
            return 0x5;
        }

        @Override
        protected int bundledInputMask(int shape) {
            return 0x5;
        }

        @Override
        protected int outputMask(int shape) {
            return 0;
        }

        @Override
        protected int inputMask(int shape) {
            return 0xA;
        }

        @Override
        protected @Nullable byte[] getBundledOutput(int r) {
            return r == 0 ? output0 : output2;
        }

        protected byte[] calcBundledInput(int r) {
            return raiseSignal(copySignal(getBundledInput(r)), getBundledOutput(r)); //OR with output
        }

        @Override
        protected void gateLogicOnChange() {
            boolean inputChanged = false;

            int oldInput = state() & 0xF;
            int newInput = getInput(10);
            if (oldInput != newInput) {
                setState(state() & 0xF0 | newInput);
                inputChanged = true;
            }

            byte[] newInput0 = calcBundledInput(0);
            if (!signalsEqual(input0, newInput0)) {
                input0 = newInput0;
                inputChanged = true;
            }

            byte[] newInput2 = calcBundledInput(2);
            if (!signalsEqual(input2, newInput2)) {
                input2 = newInput2;
                inputChanged = true;
            }

            if (inputChanged) onInputChange();
            if (!signalsEqual(output0, calcBundledOutput(0)) || !signalsEqual(output2, calcBundledOutput(2))) scheduleTick(2);
        }

        protected @Nullable byte[] calcBundledOutput(int r) {
            int input = state() & 0xF;
            if (shape() == 1) input = flipMaskZ(input);

            if (r == 0) {
                return (input & 2) != 0 ? input2 : null;
            } else if (r == 2) {
                return (input & 8) != 0 ? input0 : null;
            } else {
                return null;
            }
        }

        @Override
        protected void gateLogicOnScheduledTick() {
            output0 = calcBundledOutput(0);
            output2 = calcBundledOutput(2);
            gateLogicOnChange();
            onOutputChange(0x5);
            sendClientUpdate();
        }

        @Override
        protected boolean gateLogicCycleShape() {
            setShape(shape() == 0 ? 1 : 0);
            return true;
        }
        //endregion
    }

    public static class BusRandomizer extends BundledGatePart {

        private static final Random RANDOM = new Random();

        private static final int KEY_OUTPUT = 20;
        private static final int KEY_MASK = 21;

        private final byte[] unpackedOut = new byte[16];
        private short output = 0;
        private short mask = (short) 0xFFFF;

        public BusRandomizer(GateType type) {
            super(type);
        }

        //region save/load
        @Override
        public void save(CompoundTag tag, HolderLookup.Provider lookupProvider) {
            super.save(tag, lookupProvider);
            tag.putShort("in", mask);
            tag.putShort("out", output);
        }

        @Override
        public void load(CompoundTag tag, HolderLookup.Provider lookupProvider) {
            super.load(tag, lookupProvider);
            mask = tag.getShort("in");
            output = tag.getShort("out");
            unpackDigital(unpackedOut, output);
        }

        @Override
        public void writeDesc(MCDataOutput packet) {
            super.writeDesc(packet);
            packet.writeShort(output);
            packet.writeShort(mask);
        }

        @Override
        public void readDesc(MCDataInput packet) {
            super.readDesc(packet);
            output = packet.readShort();
            mask = packet.readShort();
        }
        //endregion

        //region Packets
        @Override
        protected void read(MCDataInput packet, int key) {
            switch (key) {
                case KEY_OUTPUT:
                    output = packet.readShort();
                    if (Configurator.staticGates) tile().markRender();
                    break;
                case KEY_MASK:
                    mask = packet.readShort();
                    if (Configurator.staticGates) tile().markRender();
                    break;
                default:
                    super.read(packet, key);
            }
        }

        protected void sendOutUpdate() {
            sendUpdate(KEY_OUTPUT, p -> p.writeShort(output));
        }

        protected void sendMaskUpdate() {
            sendUpdate(KEY_MASK, p -> p.writeShort(mask));
        }
        //endregion

        //region Render data
        @Override
        public short bOutput0() {
            return output;
        }

        @Override
        public short bInput2() {
            return mask;
        }
        //endregion

        //region Gate logic
        @Override
        protected int bundledOutputMask(int shape) {
            return 0x5;
        }

        @Override
        protected int inputMask(int shape) {
            return 0xA;
        }

        @Override
        protected int outputMask(int shape) {
            return 0;
        }

        @Override
        protected void gateLogicOnChange() {
            boolean inputChanged = false;
            int oldInput = state() & 0xF;
            int newInput = getInput(10);
            if (oldInput != newInput) {
                setState(state() & 0xF0 | newInput);
                inputChanged = true;
            }

            short newMask = (short) packDigital(getBundledInput(2));
            if (newMask == 0) newMask = (short) 0xFFFF;
            if (mask != newMask) {
                mask = newMask;
                inputChanged = true;
                sendMaskUpdate();
            }

            if (inputChanged) onInputChange();
            if (newInput != 0) scheduleTick(2);
        }

        @Override
        protected void gateLogicOnScheduledTick() {
            short oldOut = output;
            output = (state() & 0xF) != 0 ? shape() == 0 ? calc1BitOut() : calcNBitOut() : oldOut;
            if (oldOut != output) {
                unpackDigital(unpackedOut, output);
                onOutputChange(1);
                sendOutUpdate();
            }
            gateLogicOnChange();
        }

        private short calc1BitOut() {
            int high = Integer.bitCount(mask);
            int n = RANDOM.nextInt(high);
            int v = 0;
            for (int i = 0; i < 16; i++) {
                if ((mask & 1 << i) != 0 && v++ == n) {
                    return (short) (1 << i);
                }
            }
            return 0;
        }

        private short calcNBitOut() {
            short out = 0;
            for (int i = 0; i < 16; i++) {
                if ((mask & 1 << i) != 0 && RANDOM.nextBoolean()) {
                    out |= 1 << i;
                }
            }
            return out;
        }

        @Override
        protected @Nullable byte[] getBundledOutput(int r) {
            return r == 0 ? unpackedOut : null;
        }

        @Override
        protected boolean gateLogicCycleShape() {
            setShape(shape() == 0 ? 1 : 0);
            return true;
        }
        //endregion
    }

    public static class BusConverter extends BundledGatePart {

        private static final int KEY_CLIENT_IO = 20;

        private short bIn, bOut;
        private byte rsIn, rsOut;
        private final byte[] bOutUnpacked = new byte[16];

        boolean forceBInUpdate, forceBOutUpdate = false;

        public BusConverter(GateType type) {
            super(type);
        }

        private void setBOut(int newBOut) {
            if (bOut != (short) newBOut) {
                bOut = (short) newBOut;
                unpackDigital(bOutUnpacked, bOut);
            }
        }

        //region save/load
        @Override
        public void save(CompoundTag tag, HolderLookup.Provider lookupProvider) {
            super.save(tag, lookupProvider);
            tag.putByte("in", rsIn);
            tag.putByte("out", rsOut);
            tag.putShort("in0", bIn);
            tag.putShort("out0", bOut);
            tag.putBoolean("revised", true);
        }

        @Override
        public void load(CompoundTag tag, HolderLookup.Provider lookupProvider) {
            super.load(tag, lookupProvider);
            rsIn = tag.getByte("in");
            rsOut = tag.getByte("out");
            if (tag.getBoolean("revised")) {
                bIn = tag.getShort("in0");
                setBOut(tag.getShort("out0"));
            } else {
                bIn = tag.getByte("in0");
                setBOut(tag.getByte("out0"));
                forceBInUpdate = true;
                forceBOutUpdate = true;
            }
        }

        @Override
        public void writeDesc(MCDataOutput packet) {
            super.writeDesc(packet);
            packet.writeShort(packClientData());
        }

        @Override
        public void readDesc(MCDataInput packet) {
            super.readDesc(packet);
            unpackClientData(packet.readShort());
        }
        //endregion

        //region Packets
        @Override
        protected void read(MCDataInput packet, int key) {
            switch (key) {
                case KEY_CLIENT_IO:
                    unpackClientData(packet.readShort());
                    if (Configurator.staticGates) tile().markRender();
                    break;
                default:
                    super.read(packet, key);
            }
        }

        private void sendClientUpdate() {
            sendUpdate(KEY_CLIENT_IO, p -> p.writeShort(packClientData()));
        }

        private short packClientData() {
            return (short) (rsIn | rsOut << 4 | mostSignificantBit(bIn) << 8 | mostSignificantBit(bOut) << 12);
        }

        private void unpackClientData(int data) {
            rsIn = (byte) (data & 0xF);
            rsOut = (byte) (data >> 4 & 0xF);
            bIn = (short) (1 << (data >> 8 & 0xF));
            setBOut(1 << (data >> 12 & 0xF));
        }
        //endregion

        @Override
        public int rsIO() {
            return (rsIn | rsOut) & 0xFF;
        }

        //region Gate logic
        @Override
        protected int bundledOutputMask(int shape) {
            return shape == 0 ? 1 : 0;
        }

        @Override
        protected int bundledInputMask(int shape) {
            return shape == 0 ? 0 : 1;
        }

        @Override
        protected int outputMask(int shape) {
            return shape == 0 ? 0xA : 0xE;
        }

        @Override
        protected int inputMask(int shape) {
            return shape == 0 ? 4 : 0;
        }

        @Override
        protected int getOutput(int r) {
            return shape() != 0 && r == 2 ? rsOut : (state() & 0x10 << r) != 0 ? 0xF : 0;
        }

        @Override
        protected @Nullable byte[] getBundledOutput(int r) {
            return shape() == 0 && r == 0 ? bOutUnpacked : null;
        }

        @Override
        protected void gateLogicOnChange() {
            boolean changed = false;

            byte oldRSIn = rsIn;
            rsIn = (byte) (shape() == 0 ? getAnalogRedstoneInput(2) : 0);
            if (oldRSIn != rsIn) changed = true;

            short oldBIn = bIn;
            bIn = (short) (shape() == 0 ? 0 : packDigital(getBundledInput(0)));
            if (oldBIn != bIn) changed = true;

            if (changed | forceBInUpdate) {
                forceBInUpdate = false;
                onInputChange();
                scheduleTick(2);
                sendClientUpdate();
            }
        }

        @Override
        protected void gateLogicOnScheduledTick() {
            int changeMask = 0;

            short oldBOut = bOut;
            setBOut(shape() == 0 ? 1 << rsIn : 0);
            if (oldBOut != bOut | forceBOutUpdate) {
                forceBOutUpdate = false;
                changeMask |= 1;
            }

            byte oldRSOut = rsOut;
            rsOut = (byte) (shape() == 0 ? 0 : mostSignificantBit(bIn));
            if (rsOut != oldRSOut) changeMask |= 4;

            int oldOut2 = state() >> 4;
            int newOut2 = (shape() == 0 ? rsIn : bIn) != 0 ? 10 : 0;
            if (oldOut2 != newOut2) {
                setState(state() & 0xF | newOut2 << 4);
                changeMask |= 10;
            }

            if (changeMask != 0) {
                onOutputChange(changeMask);
                sendClientUpdate();
            }

            gateLogicOnChange();
        }

        @Override
        protected boolean gateLogicCycleShape() {
            setShape(shape() == 0 ? 1 : 0);
            return true;
        }
        //endregion
    }

    public static class BusInputPanel extends BundledGatePart {

        private static final int KEY_PRESS_MASK = 20;

        private static final Cuboid6[] UNPRESSED_BOXES = VecLib.buildCubeArray(4, 4, new Cuboid6(3, 1, 3, 13, 3, 13), new Vector3(-0.25, 0, -0.25));
        private static final Cuboid6[] PRESSED_BOXES = VecLib.buildCubeArray(4, 4, new Cuboid6(3, 1, 3, 13, 2.5, 13), new Vector3(-0.25, 0, -0.25));

        private static final ArrayList<HashMap<Integer, MultiIndexedVoxelShape>> shapeCache = new ArrayList<>(6 * 4);

        public static MultiIndexedVoxelShape getOrCreateOutline(int orient, short pressMask) {
            while (shapeCache.size() <= orient) shapeCache.add(null);

            HashMap<Integer, MultiIndexedVoxelShape> shapeMap = shapeCache.get(orient);
            if (shapeMap == null) {
                shapeMap = new HashMap<>();
                shapeCache.set(orient, shapeMap);
            }

            return shapeMap.computeIfAbsent(pressMask & 0xFFFF, k -> createOutline(orient, pressMask));
        }

        private static MultiIndexedVoxelShape createOutline(int orient, short pressMask) {

            Transformation t = VecLib.orientT(orient);
            List<IndexedVoxelShape> shapeList = new LinkedList<>();

            // Base platform box
            VoxelShape baseShape = VoxelShapeCache.getShape(FaceMicroblockPart.aBounds[0x10].copy().apply(t));
            shapeList.add(new IndexedVoxelShape(baseShape, -1));
            for (int i = 0; i < 16; i++) {
                Cuboid6 bounds = ((pressMask & 1 << i) != 0 ? PRESSED_BOXES : UNPRESSED_BOXES)[i].copy().apply(t);
                shapeList.add(new IndexedVoxelShape(VoxelShapeCache.getShape(bounds), i));
            }

            return new MultiIndexedVoxelShape(ImmutableSet.copyOf(shapeList));
        }

        private short pressMask = 0;
        private short bOut = 0;
        private final byte[] bOutUnpacked = new byte[16];

        public BusInputPanel(GateType type) {
            super(type);
        }

        public void setbOut(int newBOut) {
            if (bOut != (short) newBOut) {
                bOut = (short) newBOut;
                unpackDigital(bOutUnpacked, bOut);
            }
        }

        //region save/load
        @Override
        public void save(CompoundTag tag, HolderLookup.Provider lookupProvider) {
            super.save(tag, lookupProvider);
            tag.putShort("press", pressMask);
            tag.putShort("mask", bOut);
        }

        @Override
        public void load(CompoundTag tag, HolderLookup.Provider lookupProvider) {
            super.load(tag, lookupProvider);
            pressMask = tag.getShort("press");
            setbOut(tag.getShort("mask"));
        }

        @Override
        public void writeDesc(MCDataOutput packet) {
            super.writeDesc(packet);
            packet.writeShort(pressMask);
        }

        @Override
        public void readDesc(MCDataInput packet) {
            super.readDesc(packet);
            pressMask = packet.readShort();
        }
        //endregion

        //region Packets
        @Override
        protected void read(MCDataInput packet, int key) {
            switch (key) {
                case KEY_PRESS_MASK:
                    pressMask = packet.readShort();
                    if (Configurator.staticGates) tile().markRender();
                    break;
                default:
                    super.read(packet, key);
            }
        }

        private void sendClientUpdate() {
            sendUpdate(KEY_PRESS_MASK, p -> p.writeShort(pressMask));
        }
        //endregion

        //region Render data
        @Override
        public short bOutput2() {
            return bOut;
        }

        @Override
        public short bInput0() {
            return pressMask;
        }

        @Override
        public BlockPos worldPos() {
            return pos();
        }
        //endregion

        //region Gate logic
        @Override
        protected int bundledOutputMask(int shape) {
            return 4;
        }

        @Override
        protected int bundledInputMask(int shape) {
            return 0;
        }

        @Override
        protected int outputMask(int shape) {
            return 0;
        }

        @Override
        protected int inputMask(int shape) {
            return 1;
        }

        @Override
        protected int getOutput(int r) {
            //TODO same as super. Dont  override?
            return (state() & 0x10 << r) != 0 ? 15 : 0;
        }

        @Override
        protected @Nullable byte[] getBundledOutput(int r) {
            //TODO filter r == 2 ?
            return bOutUnpacked;
        }

        @Override
        protected void gateLogicOnChange() {
            boolean inputChanged = false;

            int oldInput = state() & 0xF;
            int newInput = getInput(1);
            if (oldInput != newInput) {
                setState(state() & 0xF0 | newInput);
                inputChanged = true;
            }

            if ((state() & 1) != 0) pressMask = 0;

            short oldBInput = bOut;
            short newBInput = pressMask;
            if (oldBInput != newBInput) inputChanged = true;

            if (inputChanged) {
                onInputChange();
                scheduleTick(2);
            }
        }

        @Override
        protected void gateLogicOnScheduledTick() {
            boolean outputChanged = false;

            short oldBOut = bOut;
            short newBOut = pressMask;
            if (oldBOut != newBOut) {
                setbOut(pressMask);
                outputChanged = true;
                sendClientUpdate();
            }

            if (outputChanged) onOutputChange(bundledOutputMask(shape()));
            gateLogicOnChange();
        }
        //endregion

        @Override
        public VoxelShape getShape(CollisionContext context) {
            return getOrCreateOutline(getOrientation(), pressMask);
        }

        @Override
        protected boolean gateLogicActivate(Player player, ItemStack held, PartRayTraceResult hit) {
            if (!held.isEmpty() && held.getItem() instanceof IScrewdriver) return false;
            if (hit.subHit > -1) {
                if (!level().isClientSide) {
                    pressMask ^= (1 << hit.subHit);
                    gateLogicOnChange();
                }
                return true;
            }
            return false;
        }
    }

    public static class SegmentDisplay extends BundledGatePart {

        private static final int KEY_BUNDLED_INPUT = 20;

        private short bInput0 = 0;

        public SegmentDisplay(GateType type) {
            super(type);
        }

        //region save/load
        @Override
        public void save(CompoundTag tag, HolderLookup.Provider lookupProvider) {
            super.save(tag, lookupProvider);
            tag.putShort("in", bInput0);
        }

        @Override
        public void load(CompoundTag tag, HolderLookup.Provider lookupProvider) {
            super.load(tag, lookupProvider);
            bInput0 = tag.getShort("in");
        }

        @Override
        public void writeDesc(MCDataOutput packet) {
            super.writeDesc(packet);
            packet.writeShort(bInput0);
        }

        @Override
        public void readDesc(MCDataInput packet) {
            super.readDesc(packet);
            bInput0 = packet.readShort();
        }
        //endregion

        //region Packets
        @Override
        protected void read(MCDataInput packet, int key) {
            switch (key) {
                case KEY_BUNDLED_INPUT:
                    bInput0 = packet.readShort();
                    if (Configurator.staticGates) tile().markRender();
                    break;
                default:
                    super.read(packet, key);
            }
        }

        private void sendInputUpdate() {
            sendUpdate(KEY_BUNDLED_INPUT, p -> p.writeShort(bInput0));
        }
        //endregion

        //region Render data
        @Override
        public short bInput0() {
            return bInput0;
        }
        //endregion

        //region Gate Logic
        @Override
        protected int bundledInputMask(int shape) {
            return 1;
        }

        @Override
        protected int getOutput(int r) {
            return 0; // Super derives output from state, but we store colour in that
        }

        @Override
        protected void gateLogicOnChange() {
            short newBIn = (short) packDigital(getBundledInput(0));
            if (bInput0 != newBIn) {
                bInput0 = newBIn;
                onInputChange();
                sendInputUpdate();
                scheduleTick(2);
            }
        }

        @Override
        protected void gateLogicOnScheduledTick() {
            gateLogicOnChange();
        }

        @Override
        protected void gateLogicSetup() {
            setState(EnumColour.RED.ordinal());
        }

        @Override
        protected boolean gateLogicCycleShape() {
            setShape(shape() == 0 ? 1 : 0);
            return true;
        }
        //endregion

        @Override
        protected boolean gateLogicActivate(Player player, ItemStack held, PartRayTraceResult hit) {
            if (held.isEmpty() || held.getItem() instanceof IScrewdriver) return false;
            EnumColour c = EnumColour.fromDyeStack(held);
            if (c != null && c.ordinal() != getState() && c != EnumColour.BLACK) {
                if (!level().isClientSide) {
                    setState(c.ordinal());
                    sendStateUpdate();
                }
                return true;
            }
            return false;
        }
    }
}
