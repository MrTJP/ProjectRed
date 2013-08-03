package mrtjp.projectred.transmission;

import mrtjp.projectred.core.IProjectRedModule;
import mrtjp.projectred.core.IProxy;
import cpw.mods.fml.common.network.IGuiHandler;

public class ModuleTransmission implements IProjectRedModule {

	@Override
	public IProxy getCommonProxy() {
		return new TransmissionProxy();
	}

	@Override
	public IProxy getClientProxy() {
		return new TransmissionClientProxy();
	}

	@Override
	public IGuiHandler getGuiHandler() {
		return null;
	}

	@Override
	public String getModuleID() {
		return "Transmission";
	}

	@Override
	public String[] getModuleDependencies() {
		return null;
	}

}
