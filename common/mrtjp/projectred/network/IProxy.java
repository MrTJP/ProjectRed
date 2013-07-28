package mrtjp.projectred.network;

import mrtjp.projectred.core.IProjectRedModule;


public abstract interface IProxy {
	
	public abstract void init();
	public abstract void preinit();
	public abstract void postinit();
	
	public abstract void initRenderings();

	public void registerEventsAndHandlers();
	
	public void initOreDictionaryDefinitions();

}