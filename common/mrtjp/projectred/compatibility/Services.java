package mrtjp.projectred.compatibility;

import cpw.mods.fml.common.Loader;

public class Services
{
    /** loaded interactions **/
    public static boolean loadTConstruct = false;

    public static void loadServices()
    {
        if (Loader.isModLoaded("TConstruct"))
        {
            loadTConstruct = true;
            tcProxy = new ProxyTConstruct();
        }
    }

    private static ProxyTConstruct tcProxy;

    public static ProxyTConstruct getTCProxy()
    {
        return tcProxy;
    }
}
