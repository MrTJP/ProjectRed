package mrtjp.projectred.core;

import codechicken.lib.vec.Rotation;
import codechicken.multipart.api.part.MultiPart;
import codechicken.multipart.block.TileMultipart;
import mrtjp.projectred.api.IBundledEmitter;
import mrtjp.projectred.api.IBundledTile;
import mrtjp.projectred.api.IBundledTileInteraction;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;

import javax.annotation.Nullable;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

public class BundledSignalsLib {

    // Array mapping side to base rotation of a bundled cable.
    // Indexed by Direction ordinal. Output is a quarter rotation index 0 - 3
    public static final int[] bundledCableBaseRotationMap = { 0, 3, 3, 0, 0, 3 };

    private static final List<IBundledTileInteraction> interactions = new LinkedList<>();

    //region Interactions

    public static void registerBundledTileInteraction(IBundledTileInteraction interaction) {
        interactions.add(interaction);
    }

    public static @Nullable byte[] getBundledInput(Level world, BlockPos pos, Direction facing) {

        int side = facing.ordinal();
        BlockEntity tile = world.getBlockEntity(pos.relative(facing));

        if (tile instanceof IBundledTile) {
            return ((IBundledTile) tile).getBundledSignal(side ^ 1);
        }

        if (tile instanceof TileMultipart tmp) {
            byte[] signal = null;

            // Source signal from all 4 perpendicular faces
            for (int r = 0; r < 4; r++) {
                int pside = Rotation.rotateSide(side, r);
                MultiPart part = tmp.getSlottedPart(pside);
                if (part instanceof IBundledEmitter) {
                    int pr = Rotation.rotationTo(pside, side ^ 1);
                    signal = raiseSignal(signal, ((IBundledEmitter) part).getBundledSignal(pr));
                }
            }

            // Source signal from center part
            MultiPart part = tmp.getSlottedPart(6);
            if (part instanceof IBundledEmitter) {
                signal = raiseSignal(signal, ((IBundledEmitter) part).getBundledSignal(side ^ 1));
            }

            return signal;
        }

        return null;
    }

    public static boolean canConnectBundledViaInteraction(Level world, BlockPos pos, Direction side) {
        for (IBundledTileInteraction interaction : interactions) {
            if (interaction.isValidInteractionFor(world, pos, side)) {
                return interaction.canConnectBundled(world, pos, side);
            }
        }
        return false;
    }

    public static boolean isValidInteractionFor(Level world, BlockPos pos, Direction side) {
        for (IBundledTileInteraction interaction : interactions) {
            if (interaction.isValidInteractionFor(world, pos, side)) return true;
        }
        return false;
    }

    public static @Nullable byte[] getBundledSignalViaInteraction(Level world, BlockPos pos, Direction side) {
        for (IBundledTileInteraction interaction : interactions) {
            if (interaction.isValidInteractionFor(world, pos, side)) {
                return interaction.getBundledSignal(world, pos, side);
            }
        }
        return null;
    }
    //endregion

    //region Signal utilities

    public static boolean signalsEqual(@Nullable byte[] signal1, @Nullable byte[] signal2) {
        if (signal1 == null) return isSignalZero(signal2);
        if (signal2 == null) return isSignalZero(signal1);
        if (signal1.length != signal2.length) return false;
        for (int i = 0; i < signal1.length; i++) {
            if (signal1[i] != signal2[i]) return false;
        }
        return true;
    }

    public static boolean isSignalZero(@Nullable byte[] signal) {
        if (signal == null) return true;
        for (byte b : signal) {
            if (b != 0) return false;
        }
        return true;
    }

    public static boolean isSignalZero(@Nullable byte[] signal, int mask) {
        if (signal == null) return true;
        for (int i = 0; i < signal.length; i++) {
            if ((mask & 1 << i) != 0 && signal[i] != 0) return false;
        }
        return true;
    }

    public static boolean dropSignalsLessThan(byte[] signal, byte[] threshold) {

        boolean dropped = false;
        for (int i = 0; i < 16; i++) {
            if ((threshold[i] & 0xFF) < (signal[i] & 0xFF)) {
                signal[i] = 0;
                dropped = true;
            }
        }
        return dropped;
    }

    public static void applyChangeMask(byte[] source, byte[] dest, int mask) {
        for (int i = 0; i < 16; i++) {
            if ((mask & 1 << i) == 0) {
                dest[i] = source[i];
            }
        }
    }

    public static byte[] raiseSignal(@Nullable byte[] signal, @Nullable byte[] source) {
        if (signal == null) signal = new byte[16];
        if (source == null) return signal;
        for (int i = 0; i < 16; i++) {
            if ((signal[i] & 0xFF) < (source[i] & 0xFF)) {
                signal[i] = source[i];
            }
        }
        return signal;
    }

    public static @Nullable byte[] copySignal(@Nullable byte[] signal) {
        return signal == null ? null : signal.clone();
    }

    public static void saveSignal(CompoundTag tag, String key, @Nullable byte[] signal) {
        if (signal != null) tag.putByteArray(key, signal);
    }

    public static @Nullable byte[] loadSignal(CompoundTag tag, String key) {
        if (tag.contains(key)) return tag.getByteArray(key).clone();
        return null;
    }

    public static int packDigital(@Nullable byte[] signal) {
        if (signal == null) return 0;
        int packed = 0;
        for (int i = 0; i < 16; i++) {
            if (signal[i] != 0) packed |= 1 << i;
        }
        return packed;
    }

    public static @Nullable byte[] unpackDigital(@Nullable byte[] signal, int packed) {
        if (packed == 0) {
            if (signal != null) Arrays.fill(signal, (byte) 0);
            return signal;
        }
        if (signal == null) signal = new byte[16];
        for (int i = 0; i < 16; i++) {
            signal[i] = (packed & 1 << i) != 0 ? (byte) 255 : 0;
        }
        return signal;
    }

    public static int mostSignificantBit(int mask) {
        int idx = 0;
        int m2 = mask >>> 1;
        while (m2 != 0) {
            m2 >>>= 1;
            idx++;
        }
        return idx;
    }

    public static String signalToString(byte[] signal) {
        if (isSignalZero(signal)) return "0";
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < 16; i++) {
            sb.append("[").append(signal[i] & 0xFF).append("]");
        }
        return sb.toString();
    }
    //endregion
}
