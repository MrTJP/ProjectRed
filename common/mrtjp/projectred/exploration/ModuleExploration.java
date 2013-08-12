package mrtjp.projectred.exploration;

import cpw.mods.fml.common.network.IGuiHandler;
import mrtjp.projectred.core.IProjectRedModule;
import mrtjp.projectred.core.IProxy;
import mrtjp.projectred.core.ModuleCore;

public class ModuleExploration implements IProjectRedModule {

    @Override
    public IProxy getCommonProxy() {
        return new ExplorationProxy();
    }

    @Override
    public IProxy getClientProxy() {
        return new ExplorationClientProxy();
    }

    @Override
    public IGuiHandler getGuiHandler() {
        return null;
    }

    @Override
    public String getModuleID() {
        return "Exploration";
    }

    @Override
    public String[] getModuleDependencies() {
        return new String[] {new ModuleCore().getModuleID()};
    }

}
