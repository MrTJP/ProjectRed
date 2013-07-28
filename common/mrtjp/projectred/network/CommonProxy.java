package mrtjp.projectred.network;

import mrtjp.projectred.blocks.BlockLamp.EnumLamp;
import mrtjp.projectred.core.IProjectRedModule;
import mrtjp.projectred.integration.EnumGate;
import mrtjp.projectred.integration.ModuleIntegration;
import mrtjp.projectred.items.ItemPart.EnumPart;
import mrtjp.projectred.multipart.wiring.wires.EnumWire;

public class CommonProxy implements IProxy {

	@Override
	public void initRenderings() {
		//Client only initialization.
	}

	@Override
	public void registerEventsAndHandlers() {
		//TickRegistry.registerTickHandler(ProjectRedTickHandler.instance, Side.SERVER);
	}

	@Override
	public void initOreDictionaryDefinitions() {
		EnumLamp.initOreDictDefinitions();
		EnumWire.initOreDictDefinitions();
		EnumGate.initOreDictDefinitions();
		EnumPart.initOreDictDefinitions();
	}

	@Override
	public void init() {
		IProjectRedModule m = new ModuleIntegration();
		m.getCommonProxy().init();
	}

	@Override
	public void preinit() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void postinit() {
		// TODO Auto-generated method stub
		
	}
}