package mrtjp.projectred.expansion;

import mrtjp.projectred.core.IProjectRedModule;
import mrtjp.projectred.core.IProxy;
import mrtjp.projectred.core.ModuleCore;
import cpw.mods.fml.common.network.IGuiHandler;

public class ModuleExpansion implements IProjectRedModule {

	@Override
	public IProxy getCommonProxy() {
		return new ExpansionProxy();
	}

	@Override
	public IProxy getClientProxy() {
		return new ExpansionClientProxy();
	}

	@Override
	public IGuiHandler getGuiHandler() {
		return new ExpansionGuiHandler();
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
