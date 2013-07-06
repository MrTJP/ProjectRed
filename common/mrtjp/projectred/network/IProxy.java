package mrtjp.projectred.network;


public abstract interface IProxy {
	public abstract void initRenderings();

	public void registerEventsAndHandlers();
	
	public void initOreDictionaryDefinitions();

}