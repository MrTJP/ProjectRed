package mrtjp.projectred.compatibility.computercraft

import codechicken.lib.vec.BlockCoord
import dan200.computercraft.api.redstone.IBundledRedstoneProvider
import dan200.computercraft.api.{ComputerCraftAPI => CCAPI}
import mrtjp.projectred.api.{IBundledTileInteraction, ProjectRedAPI => PRAPI}
import mrtjp.projectred.compatibility.IPRPlugin
import mrtjp.projectred.transmission.BundledCommons
import net.minecraft.world.World

object PluginCC_BundledCable extends IPRPlugin
{
    override def getModIDs = Array("ComputerCraft", "ProjRed|Transmission")

    override def preInit(){}

    override def init()
    {
        CCAPI.registerBundledRedstoneProvider(new CCPRBundledRedstoneProvider)
        PRAPI.transmissionAPI.registerBundledTileInteraction(new PRCCBundledTileInteraction)
    }

    override def postInit(){}

    override def desc() = "Computercraft bundled cable connections"
}

class CCPRBundledRedstoneProvider extends IBundledRedstoneProvider
{
    override def getBundledRedstoneOutput(world:World, x:Int, y:Int, z:Int, side:Int) =
    {
        val pos = new BlockCoord(x, y, z).offset(side)
        val sig = PRAPI.transmissionAPI.getBundledInput(world, pos.x, pos.y, pos.z, side^1)
        BundledCommons.packDigital(sig)
    }
}

class PRCCBundledTileInteraction extends IBundledTileInteraction
{
    override def isValidInteractionFor(world:World, x:Int, y:Int, z:Int) =
        CCAPI.getBundledRedstoneOutput(world, x, y, z, 0) > -1

    override def canConnectBundled(world:World, x:Int, y:Int, z:Int, side:Int) = true

    override def getBundledSignal(world:World, x:Int, y:Int, z:Int, side:Int) =
    {
        val sig = CCAPI.getBundledRedstoneOutput(world, x, y, z, side)
        BundledCommons.unpackDigital(null, sig)
    }
}