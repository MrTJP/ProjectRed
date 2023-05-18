package mrtjp.projectred.compatibility;

import dan200.computercraft.api.ComputerCraftAPI;
import dan200.computercraft.api.redstone.IBundledRedstoneProvider;
import mrtjp.projectred.core.ProjectRedCore;
import mrtjp.projectred.api.IBundledTileInteraction;
import mrtjp.projectred.api.ProjectRedAPI;
import mrtjp.projectred.core.BundledSignalsLib;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.Level;

import javax.annotation.Nonnull;

public class ComputerCraftCompatibility {

    public static void init(Object ccModObject) {

        if (ProjectRedAPI.transmissionAPI != null) {
            ProjectRedCore.LOGGER.info("Loading Project Red ComputerCraft Compatibility Module");
            ComputerCraftAPI.registerBundledRedstoneProvider(new CCPRBundledRedstoneProvider());
            ProjectRedAPI.transmissionAPI.registerBundledTileInteraction(new PRCCBundledTileInteraction());
        }
    }

    /**
     * This is used by ComputerCraft to query bundled signals from third-party entities
     */
    private static class CCPRBundledRedstoneProvider implements IBundledRedstoneProvider {
        @Override
        public int getBundledRedstoneOutput(@Nonnull Level world, @Nonnull BlockPos pos, @Nonnull Direction side) {
            byte[] signal = ProjectRedAPI.transmissionAPI.getBundledInput(world, pos.relative(side), side.getOpposite());
            return BundledSignalsLib.packDigital(signal);
        }
    }

    private static class PRCCBundledTileInteraction implements IBundledTileInteraction {
        @Override
        public boolean isValidInteractionFor(Level world, BlockPos pos, Direction side) {
            return ComputerCraftAPI.getBundledRedstoneOutput(world, pos, side) != -1;
        }

        @Override
        public boolean canConnectBundled(Level world, BlockPos pos, Direction side) {
            return true;
        }

        @Override
        public byte[] getBundledSignal(Level world, BlockPos pos, Direction side) {
            int signal = ComputerCraftAPI.getBundledRedstoneOutput(world, pos, side);
            return BundledSignalsLib.unpackDigital(null, signal);
        }
    }
}
