package mrtjp.projectred.compatibility

import cpw.mods.fml.common.Loader
import mrtjp.projectred.compatibility.tconstruct.ProxyTConstruct
import mrtjp.projectred.compatibility.thermalexpansion.ProxyThermalExpansion

object Services
{
    /** loaded interactions **/
    var loadTConstruct = false
    var loadTExpansion = false

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
    }

    var tcProxy:ProxyTConstruct = null
    var teProxy:ProxyThermalExpansion = null
}