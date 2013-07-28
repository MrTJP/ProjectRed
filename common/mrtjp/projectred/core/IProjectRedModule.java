package mrtjp.projectred.core;

import mrtjp.projectred.network.IProxy;

public interface IProjectRedModule {
	public IProxy getCommonProxy();
	public IProxy getClientProxy();
}
