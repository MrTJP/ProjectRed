package mrtjp.projectred.network;

import mrtjp.projectred.blocks.BlockLamp.EnumLamp;
import mrtjp.projectred.core.ProjectRedTickHandler;
import mrtjp.projectred.items.ItemPart.EnumPart;
import mrtjp.projectred.multipart.wiring.gates.EnumGate;
import mrtjp.projectred.multipart.wiring.wires.EnumWire;
import cpw.mods.fml.common.registry.TickRegistry;
import cpw.mods.fml.relauncher.Side;

public class CommonProxy implements IProxy {

	@Override
	public void initRenderings() {
		//Client only initialization.
	}

	@Override
	public void registerEventsAndHandlers() {
		TickRegistry.registerTickHandler(ProjectRedTickHandler.instance, Side.SERVER);
	}

	@Override
	public void initOreDictionaryDefinitions() {
		EnumLamp.initOreDictDefinitions();
		EnumWire.initOreDictDefinitions();
		EnumGate.initOreDictDefinitions();
		
		EnumPart.initOreDictDefinitions();
	}
	
	

}