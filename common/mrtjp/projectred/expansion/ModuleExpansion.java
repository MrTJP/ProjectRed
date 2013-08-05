package mrtjp.projectred.expansion;

import cpw.mods.fml.common.network.IGuiHandler;
import mrtjp.projectred.core.IProjectRedModule;
import mrtjp.projectred.core.IProxy;
import mrtjp.projectred.core.ModuleCore;

public class ModuleExpansion implements IProjectRedModule {

	@Override
	public IProxy getCommonProxy() {
		return null;
	}

	@Override
	public IProxy getClientProxy() {
		return null;
	}

	@Override
	public IGuiHandler getGuiHandler() {
		return null;
	}

	@Override
	public String getModuleID() {
		return "Expansion";
	}

	@Override
	public String[] getModuleDependencies() {
		return new String[] {new ModuleCore().getModuleID()};
	}

}
