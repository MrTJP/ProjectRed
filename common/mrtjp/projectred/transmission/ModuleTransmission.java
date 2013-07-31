package mrtjp.projectred.transmission;

import cpw.mods.fml.common.network.IGuiHandler;
import mrtjp.projectred.core.IProjectRedModule;
import mrtjp.projectred.network.IProxy;

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
