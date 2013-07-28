package mrtjp.projectred.integration;

import mrtjp.projectred.core.IProjectRedModule;
import mrtjp.projectred.network.IProxy;

public class ModuleIntegration implements IProjectRedModule {

	@Override
	public IProxy getCommonProxy() {
		return new IntegrationProxy();
	}

	@Override
	public IProxy getClientProxy() {
		return new IntegrationClientProxy();
	}

}
