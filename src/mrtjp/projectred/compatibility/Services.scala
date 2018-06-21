package mrtjp.projectred.compatibility

import mrtjp.projectred.ProjectRedCore
import mrtjp.projectred.compatibility.chisel.PluginChisel
import mrtjp.projectred.compatibility.computercraft.PluginCC_BundledCable
import mrtjp.projectred.compatibility.treecapitator.PluginTreecapitator
import net.minecraftforge.fml.common.Loader

object Services
{
    //Hardcoded list of all possible plugins
    val rootPlugins = Seq[IPRPlugin](
        PluginCC_BundledCable,
        PluginTreecapitator,
        PluginChisel
//        PluginTConstruct,
//        PluginThermalExpansion,
//        PluginColoredLights,
//        PluginMFRDeepStorage
    )

    //List of all loaded plugins
    var plugins = Seq[IPRPlugin]()

    def servicesLoad()
    {
        try {
            for (p <- rootPlugins)
                if (p.isEnabled && p.getModIDs.forall(Loader.isModLoaded))
                    plugins :+= p
                else
                    ProjectRedCore.log.warn(p.loadFailedDesc())
        }
        catch {
            case e:Exception =>
        }
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