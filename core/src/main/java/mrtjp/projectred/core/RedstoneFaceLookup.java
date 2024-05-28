package mrtjp.projectred.core;

import codechicken.multipart.api.RedstoneInteractions;
import codechicken.multipart.api.part.MultiPart;
import codechicken.multipart.api.part.redstone.FaceRedstonePart;
import mrtjp.projectred.core.part.IRedwireEmitter;
import mrtjp.projectred.core.part.IRedwirePart;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.RedStoneWireBlock;

public class RedstoneFaceLookup {

    public static int resolveSignal(FaceLookup lookup, boolean diminishRedwire) {

        if (lookup.part instanceof IRedwirePart rp) {
            int signal = rp.getRedwireSignal(lookup.otherRotation);
            if (diminishRedwire && rp.diminishOnSide(lookup.otherRotation)) {
                signal--;
            }
            return Math.max(0, signal);
        }

        if (lookup.part instanceof IRedwireEmitter rwe) {
            return rwe.getRedwireSignal(lookup.otherRotation);
        }

        if (lookup.part instanceof FaceRedstonePart fp) {
            int s = lookup.otherDir;
            return Math.max(fp.strongPowerLevel(s), fp.weakPowerLevel(s)) * 17;
        }

        return 0;
    }

    public static int resolveVanillaSignal(FaceLookup lookup, MultiPart part, boolean strong, boolean limitDust) {
        int signal;

        // Dust signal
        if (lookup.block == Blocks.REDSTONE_WIRE) {
            signal = Math.max(lookup.state.getValue(RedStoneWireBlock.POWER) - 1, 0);
            if (limitDust) {
                return signal;
            }
        }

        // Strong signal
        return RedstoneInteractions.getPowerTo(part, lookup.dir) * 17;

        // TODO Investigate below. I think RedstoneInteractions already takes care of strong/weak check,
        //      as it is part of Level#getSignal now.
//        // Strong signal
//        signal = RedstoneInteractions.getPowerTo(part, lookup.dir) * 17;
//        if (signal > 0 || strong) {
//            return signal;
//        }
//
//        // Weak signal
//        if (lookup.state.isRedstoneConductor(lookup.world, lookup.otherPos)) {
//            signal = lookup.world.getBestNeighborSignal(lookup.otherPos) * 17;
//        }
//
//        return signal;
    }

}
