package mrtjp.projectred.compatibility.computercraft

import dan200.computercraft.api.redstone.IBundledRedstoneProvider
import dan200.computercraft.api.{ComputerCraftAPI => CCAPI}
import mrtjp.projectred.api.{IBundledTileInteraction, ProjectRedAPI => PRAPI}
import mrtjp.projectred.compatibility.IPRPlugin
import mrtjp.projectred.core.Configurator
import mrtjp.projectred.transmission.BundledCommons
import net.minecraft.util.EnumFacing
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World
import net.minecraftforge.fml.common.Optional.{Interface, Method}

object PluginCC_BundledCable extends IPRPlugin
{
    override def getModIDs = Array("computercraft", "projectred-transmission")

    override def isEnabled = Configurator.compat_CCBundledCalbe

    override def preInit(){}

    override def init()
    {
        CCPRBundledRedstoneProvider.register()
        PRCCBundledTileInteraction.register()
    }

    override def postInit(){}

    override def desc() = "ComputerCraft: bundled cable connections"
}

@Interface(modid = "computercraft", iface = "dan200.computercraft.api.redstone.IBundledRedstoneProvider", striprefs = true)
object CCPRBundledRedstoneProvider extends IBundledRedstoneProvider
{
    @Method(modid = "computercraft")
    def register()
    {
        CCAPI.registerBundledRedstoneProvider(this)
    }

    @Method(modid = "computercraft")
    override def getBundledRedstoneOutput(world:World, pos:BlockPos, side:EnumFacing) =
    {
        val sig = PRAPI.transmissionAPI.getBundledInput(world, pos.offset(side), side.getOpposite)
        BundledCommons.packDigital(sig)
    }
}

@Interface(modid = "projectred-transmission", iface = "mrtjp.projectred.api.IBundledTileInteraction", striprefs = true)
object PRCCBundledTileInteraction extends IBundledTileInteraction
{
    @Method(modid = "projectred-transmission")
    def register()
    {
        PRAPI.transmissionAPI.registerBundledTileInteraction(this)
    }

    @Method(modid = "projectred-transmission")
    override def isValidInteractionFor(world:World, pos:BlockPos, side:EnumFacing) =
        CCAPI.getBundledRedstoneOutput(world, pos, side) > -1

    @Method(modid = "projectred-transmission")
    override def canConnectBundled(world:World, pos:BlockPos, side:EnumFacing) = true

    @Method(modid = "projectred-transmission")
    override def getBundledSignal(world:World, pos:BlockPos, side:EnumFacing) =
    {
        val sig = CCAPI.getBundledRedstoneOutput(world, pos, side)
        BundledCommons.unpackDigital(null, sig)
    }
}