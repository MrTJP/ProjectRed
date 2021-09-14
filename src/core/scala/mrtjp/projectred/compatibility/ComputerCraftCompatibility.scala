package mrtjp.projectred.compatibility

import dan200.computercraft.api.ComputerCraftAPI
import dan200.computercraft.api.redstone.IBundledRedstoneProvider
import mrtjp.projectred.api.{IBundledTileInteraction, ProjectRedAPI}
import mrtjp.projectred.core.BundledCommons
import net.minecraft.util.Direction
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World

/**
 * Provides bundled cable compatibility with ComputerCraft
 */
object ComputerCraftCompatibility
{
    def init():Unit = {
        if (ProjectRedAPI.transmissionAPI != null) { // Check if Transmission is installed
            println("Loading ProjectRed compatibility: ComputerCraft")
            ComputerCraftAPI.registerBundledRedstoneProvider(new CCPRBundledRedstoneProvider)
            ProjectRedAPI.transmissionAPI.registerBundledTileInteraction(new PRCCBundledTileInteraction)
        }
    }

    /**
     * This is used by ComputerCraft to query bundled signals from third-party entities
     */
    class CCPRBundledRedstoneProvider extends IBundledRedstoneProvider
    {
        override def getBundledRedstoneOutput(world:World, pos:BlockPos, side:Direction):Int = {
            val sig = ProjectRedAPI.transmissionAPI.getBundledInput(world, pos.relative(side), side.getOpposite)
            BundledCommons.packDigital(sig)
        }
    }

    /**
     * On the other side, this is used by ProjectRed to query bundled signals from third-party entities
     */
    class PRCCBundledTileInteraction extends IBundledTileInteraction
    {
        override def isValidInteractionFor(world:World, pos:BlockPos, side:Direction):Boolean =
            ComputerCraftAPI.getBundledRedstoneOutput(world, pos, side) > -1

        override def canConnectBundled(world:World, pos:BlockPos, side:Direction):Boolean = true

        override def getBundledSignal(world:World, pos:BlockPos, side:Direction):Array[Byte] = {
            val sig = ComputerCraftAPI.getBundledRedstoneOutput(world, pos, side)
            BundledCommons.unpackDigital(null, sig)
        }
    }
}