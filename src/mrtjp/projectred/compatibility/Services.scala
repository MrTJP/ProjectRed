package mrtjp.projectred.compatibility

import cpw.mods.fml.common.Loader
import mrtjp.projectred.ProjectRedCore
import mrtjp.projectred.compatibility.computercraft.PluginCC_BundledCable
import mrtjp.projectred.compatibility.cptcoloredlights.PluginColoredLights
import mrtjp.projectred.compatibility.mfr.PluginMFRDeepStorage
import mrtjp.projectred.compatibility.storagedrawers.PluginStorageDrawers
import mrtjp.projectred.compatibility.tconstruct.PluginTConstruct
import mrtjp.projectred.compatibility.thermalexpansion.PluginThermalExpansion
import mrtjp.projectred.compatibility.treecapitator.PluginTreecapitator

object Services
{
    //Hardcoded list of all possible plugins
    val rootPlugins = Seq[IPRPlugin](
        PluginTreecapitator,
        PluginTConstruct,
        PluginThermalExpansion,
        PluginCC_BundledCable,
        PluginColoredLights,
        PluginMFRDeepStorage,
        PluginStorageDrawers
    )

    //List of all loaded plugins
    var plugins = Seq[IPRPlugin]()

    def servicesLoad()
    {
        try
        {
            for (p <- rootPlugins)
                if (p.isEnabled && p.getModIDs.forall(Loader.isModLoaded))
                {
                    plugins :+= p
                }
                else ProjectRedCore.log.warn(p.loadFailedDesc())
        }
        catch {case e:Exception =>}
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