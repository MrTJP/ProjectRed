package mrtjp.projectred.core;

import cpw.mods.fml.common.network.IGuiHandler;

public class ModuleCore implements IProjectRedModule {

    @Override
    public IProxy getCommonProxy() {
        return new CoreProxy();
    }

    @Override
    public IProxy getClientProxy() {
        return new CoreClientProxy();
    }

    @Override
    public IGuiHandler getGuiHandler() {
        return null;
    }

    @Override
    public String getModuleID() {
        return "Core";
    }

    @Override
    public String[] getModuleDependencies() {
        return null;
    }

}
