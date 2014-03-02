package mrtjp.projectred.compatibility

import cpw.mods.fml.common.Loader
import mrtjp.projectred.compatibility.tconstruct.ProxyTConstruct
import mrtjp.projectred.compatibility.thermalexpansion.ProxyThermalExpansion
import mrtjp.projectred.compatibility.treecapitator.ProxyTreecapitator

object Services
{
    /** loaded interactions **/
    var loadTConstruct = false
    var loadTExpansion = false
    var loadTreecapitator = false

    def loadServices()
    {
        if (Loader.isModLoaded("TConstruct"))
        {
            loadTConstruct = true
            tcProxy = new ProxyTConstruct
        }
        if (Loader.isModLoaded("ThermalExpansion"))
        {
            loadTExpansion = true
            teProxy = new ProxyThermalExpansion
        }
        if (Loader.isModLoaded("TreeCapitator") && Loader.isModLoaded("ProjRed|Exploration"))
        {
            loadTreecapitator = true
            treecapProxy = new ProxyTreecapitator
        }
    }

    var tcProxy:ProxyTConstruct = null
    var teProxy:ProxyThermalExpansion = null
    var treecapProxy:ProxyTreecapitator = null
}