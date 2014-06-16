package mrtjp.projectred.compatibility

import cpw.mods.fml.common.Loader
import mrtjp.projectred.core.PRLogger
import mrtjp.projectred.compatibility.treecapitator.PluginTreecapitator
import mrtjp.projectred.compatibility.computercraft.PluginComputerCraft

object Services
{
    var plugins = Seq[IPRPlugin]()

    def servicesLoad()
    {
        PRLogger.info("Started loading ProjectRed compat plugins")
        try
        {
            val rootPlugins = Seq[IPRPlugin](PluginTreecapitator, PluginComputerCraft)
            for (p <- rootPlugins)
                if (Loader.isModLoaded(p.getModID)) plugins :+= p
                else PRLogger.warn("Failed to load PR Plugin for "+p.getModID)
        }
        catch {case e:Exception =>}
        PRLogger.info("Finished loading ProjectRed compat plugins")
    }

    def doPreInit()
    {
        for (p <- plugins) p.preInit()
    }

    def doInit()
    {
        for (p <- plugins) p.init()
    }

    def doPostInit()
    {
        for (p <- plugins) p.postInit()
    }
}