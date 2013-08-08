package mrtjp.projectred.core;

import cpw.mods.fml.common.network.IGuiHandler;

public interface IProjectRedModule {
    /**
     * Get common proxy.
     * @return Proxy with client and server code.
     */
    public IProxy getCommonProxy();
    
    /**
     * Get client proxy.
     * @return Proxy with client specific code.
     */
    public IProxy getClientProxy();
    
    /**
     * Get the guiHandler instance for this module.
     * @return
     */
    public IGuiHandler getGuiHandler();
    
    /**
     * Used to determine the ID of the module.
     * @return
     */
    public String getModuleID();
    
    /**
     * A string array of the module IDs of required modules. 
     * @return
     */
    public String[] getModuleDependencies();
}

