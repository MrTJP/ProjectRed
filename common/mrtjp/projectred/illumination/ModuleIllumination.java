package mrtjp.projectred.illumination;

import mrtjp.projectred.core.IProjectRedModule;
import mrtjp.projectred.core.IProxy;
import mrtjp.projectred.core.ModuleCore;
import cpw.mods.fml.common.network.IGuiHandler;

public class ModuleIllumination implements IProjectRedModule {

    @Override
    public IProxy getCommonProxy() {
        return new IlluminationProxy();
    }

    @Override
    public IProxy getClientProxy() {
        return new IlluminationClientProxy();
    }

    @Override
    public IGuiHandler getGuiHandler() {
        return null;
    }

    @Override
    public String getModuleID() {
        return "Illumination";
    }

    @Override
    public String[] getModuleDependencies() {
        return new String[] {new ModuleCore().getModuleID()};
    }

}
