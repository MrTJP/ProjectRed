package mrtjp.projectred.compatibility;

import mrtjp.projectred.compatibility.tconstruct.ProxyTConstruct;
import mrtjp.projectred.compatibility.thermalexpansion.ProxyThermalExpansion;
import cpw.mods.fml.common.Loader;

public class Services
{
    /** loaded interactions **/
    public static boolean loadTConstruct = false;
    public static boolean loadTExpansion = false;

    public static void loadServices()
    {
        if (Loader.isModLoaded("TConstruct"))
        {
            loadTConstruct = true;
            tcProxy = new ProxyTConstruct();
        }
        
        if (Loader.isModLoaded("ThermalExpansion"))
        {
            loadTExpansion = true;
            teProxy = new ProxyThermalExpansion();
        }
    }

    private static ProxyTConstruct tcProxy;
    public static ProxyTConstruct getTCProxy()
    {
        return tcProxy;
    }
    
    private static ProxyThermalExpansion teProxy;
    public static ProxyThermalExpansion getTEProxy()
    {
        return teProxy;
    }
}
