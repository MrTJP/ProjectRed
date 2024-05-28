package mrtjp.projectred.core;

import codechicken.multipart.api.RedstoneInteractions;
import codechicken.multipart.api.part.MultiPart;
import codechicken.multipart.api.part.redstone.RedstonePart;
import mrtjp.projectred.core.part.IRedwireEmitter;
import mrtjp.projectred.core.part.IRedwirePart;

public class RedstoneCenterLookup {

    public static int resolveSignal(CenterLookup lookup, boolean diminishRedwire) {
        if (lookup.part instanceof IRedwirePart rw) {

            int signal = rw.getRedwireSignal(lookup.otherDirection);
            if (diminishRedwire && rw.diminishOnSide(lookup.otherDirection)) {
                signal--;
            }
            return Math.max(0, signal);
        }

        if (lookup.part instanceof IRedwireEmitter rwe) {
            return rwe.getRedwireSignal(lookup.otherDirection);
        }

        if (lookup.part instanceof RedstonePart rw) {
            return Math.max(rw.strongPowerLevel(lookup.otherDirection), rw.weakPowerLevel(lookup.otherDirection));
        }

        return 0;
    }

    public static int resolveVanillaSignal(CenterLookup lookup, MultiPart part) {
        return RedstoneInteractions.getPowerTo(part, lookup.direction) * 17;
    }
}
