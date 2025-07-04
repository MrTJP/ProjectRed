package mrtjp.projectred.illumination.tile;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.vec.Rotation;
import mrtjp.projectred.api.IBundledEmitter;
import mrtjp.projectred.api.IBundledTile;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.api.IMaskedBundledTile;
import mrtjp.projectred.core.BundledSignalsLib;
import mrtjp.projectred.core.CenterLookup;
import mrtjp.projectred.core.FaceLookup;
import mrtjp.projectred.core.tile.BaseConnectableBlockEntity;
import mrtjp.projectred.core.tile.IOrientableBlockEntity;
import mrtjp.projectred.illumination.block.IllumarSmartLampBlock;
import mrtjp.projectred.illumination.init.IlluminationBlocks;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.state.BlockState;

import javax.annotation.Nullable;
import java.util.Objects;

public class IllumarSmartLampBlockEntity extends BaseConnectableBlockEntity implements IOrientableBlockEntity, IMaskedBundledTile {

    private static final int PACKET_SIGNAL = 10;

    private final byte[] signal = new byte[16];

    public IllumarSmartLampBlockEntity(BlockPos pos, BlockState state) {
        super(IlluminationBlocks.ILLUMAR_SMART_LAMP_BLOCK_ENTITY.get(), pos, state);
    }

    public byte[] getSignal() {
        return signal;
    }

    @Override
    public void saveToNBT(CompoundTag tag, HolderLookup.Provider lookupProvider) {
        super.saveToNBT(tag, lookupProvider);
        tag.putByteArray("signal", signal);
    }

    @Override
    public void loadFromNBT(CompoundTag tag, HolderLookup.Provider lookupProvider) {
        super.loadFromNBT(tag, lookupProvider);

        var s = tag.getByteArray("signal");
        if (s.length == 16)
            System.arraycopy(s, 0, signal, 0, 16);
    }

    @Override
    public void writeDesc(MCDataOutput out) {
        super.writeDesc(out);
        for (int i = 0; i < 16; i++)
            out.writeByte(signal[i]);
    }

    @Override
    public void readDesc(MCDataInput in) {
        super.readDesc(in);
        for (int i = 0; i < 16; i++)
            signal[i] = in.readByte();
    }

    @Override
    public void receiveUpdateFromServer(int key, MCDataInput input) {
        switch (key) {
            case PACKET_SIGNAL -> readSignalUpdate(input);
            default -> super.receiveUpdateFromServer(key, input);
        }
    }

    private void readSignalUpdate(MCDataInput input) {
        short changeMask = input.readShort();
        for (int i = 0; i < 16; i++) {
            if ((changeMask & (1 << i)) != 0) {
                signal[i] = input.readByte();
            }
        }
    }

    private void sendSignalUpdate(int changeMask) {
        if (changeMask == 0) return;

        sendUpdateToPlayersWatchingChunk(PACKET_SIGNAL, stream -> {
            stream.writeShort(changeMask);
            for (int i = 0; i < 16; i++) {
                if ((changeMask & (1 << i)) != 0) {
                    stream.writeByte(signal[i]);
                }
            }
        });
    }

    @Override
    public BlockState storeBlockState(BlockState defaultState) {
        int max = 0;
        for (byte b : signal) {
            max = Math.max(max, b & 0xFF);
        }
        return super.storeBlockState(defaultState)
                .setValue(IllumarSmartLampBlock.LEVEL, max / 17); // 255 -> 15
    }

    //region IMaskedBundled tile
    @Override
    public byte[] getBundledSignal(int dir) {
        return null; // Block is input only
    }

    @Override
    public boolean canConnectBundled(int side) {
        return side != (getSide() ^ 1);
    }

    @Override
    public int getConnectionMask(int side) {
        if (side == getSide()) {
            return 0x1F; // All edges and center
        } if (side == (getSide() ^ 1)) {
            return 0; // No connections on top
        } else {
            return 1 << Rotation.rotationTo(side ^ 1, getSide()); // Bottom edge
        }
    }
    //endregion

    @Override
    public boolean canConnectPart(IConnectable part, int s, int edgeRot) {
        if (!(part instanceof IBundledEmitter)) return false;

        int side = getSide();

        if (s == side) { // Bottom side can input from all 4 edges and center
            return true;
        }
        if (s == (side ^ 1)) { // Top side cannot input at all
            return false;
        }

        return edgeRot == Rotation.rotationTo(s ^ 1, side); // Other sides input on edge touching bottom side
    }

    @Override
    public void onNeighborBlockChanged(BlockPos neighborPos) {
        super.onNeighborBlockChanged(neighborPos);

        if (!getLevel().isClientSide) {
            checkSignal();
        }
    }

    @Override
    public void onBlockPlaced(@Nullable LivingEntity player, ItemStack item) {
        super.onBlockPlaced(player, item);

        if (!getLevel().isClientSide) {
            checkSignal();
        }
    }

    @Override
    public void onOrientationChange() {
        if (!getLevel().isClientSide) {
            updateExternals();
            notifyExternals(0xF);
            checkSignal();
        }
    }

    private void checkSignal() {
        byte[] newSig = calcBundledInput();
        // Send update if any byte is different
        int changeMask = BundledSignalsLib.applyAndGetChangeMask(newSig, signal);
        if (changeMask != 0) {
            pushBlockState();
            sendSignalUpdate(changeMask);
        }
    }

    //region Signal acquisition
    protected byte[] calcBundledInput() {
        byte[] newSignal = new byte[16];

        for (int s = 0; s < 6; s++) {
            if (s == (getSide() ^ 1)) { // Cant connect on top
                continue;
            }

            if (s == getSide()) { // Bottom can connect straight or on edges
                // Center connection
                if (maskConnectsStraightCenter(s)) {
                    BundledSignalsLib.raiseSignal(newSignal, calcCenterSignal(s));
                }

                // Edge connections
                for (int r = 0; r < 4; r++) {
                    if (maskConnectsStraight(s, r)) { // Straight down
                        BundledSignalsLib.raiseSignal(newSignal, calcStraightSignal(s, r));
                    } else if (maskConnectsCorner(s, r)) { // Corner towards sides
                        BundledSignalsLib.raiseSignal(newSignal, calcCornerSignal(s, r));
                    }
                }

                continue;
            }

            // Perpendicular faces can connect only on bottom edge
            int r = Rotation.rotationTo(s ^ 1, getSide());
            if (maskConnectsStraight(s, r)) {
                BundledSignalsLib.raiseSignal(newSignal, calcStraightSignal(s, r));
            } else if (maskConnectsCorner(s, r)) {
                BundledSignalsLib.raiseSignal(newSignal, calcCornerSignal(s, r));
            }
        }

        return newSignal;
    }

    private @Nullable byte[] calcCornerSignal(int s, int r) {
        int vs = Rotation.rotateSide(s ^ 1, r); // virtual internal face
        int vr = Rotation.rotationTo(vs, s); // virtual rotation
        FaceLookup lookup = FaceLookup.lookupCorner(getLevel(), getBlockPos(), vs, vr);
        return resolveArray(lookup);
    }

    private @Nullable byte[] calcStraightSignal(int s, int r) {
        int vs = Rotation.rotateSide(s ^ 1, r); // virtual internal face
        int vr = Rotation.rotationTo(vs, s); // virtual rotation
        FaceLookup lookup = FaceLookup.lookupStraight(getLevel(), getBlockPos(), vs, vr);
        return resolveArray(lookup);
    }

    private @Nullable byte[] calcCenterSignal(int s) {
        CenterLookup lookup = CenterLookup.lookupStraightCenter(getLevel(), getBlockPos(), s);
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

    protected @Nullable byte[] resolveArray(CenterLookup lookup) {
        if (lookup.part instanceof IBundledEmitter) {
            return ((IBundledEmitter) lookup.part).getBundledSignal(lookup.otherDirection);

        } else if (lookup.tile instanceof IBundledTile) {
            return ((IBundledTile) lookup.tile).getBundledSignal(lookup.otherDirection);

        } else if (lookup.tile != null) {
            return BundledSignalsLib.getBundledSignalViaInteraction(Objects.requireNonNull(lookup.tile.getLevel()), lookup.tile.getBlockPos(), Direction.values()[lookup.otherDirection]);
        }
        return null;
    }
    //endregion
}
