package mrtjp.projectred.integration;

import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.network.IProxy;
import codechicken.multipart.MultiPartRegistry.IPartFactory;
import codechicken.multipart.MultiPartRegistry;
import codechicken.multipart.TMultiPart;

import static mrtjp.projectred.ProjectRed.*;

public class IntegrationProxy implements IProxy, IPartFactory {

	
	@Override
	public void init() {
        MultiPartRegistry.registerParts(this, new String[]{
                "projred-gate",
        });
        
        itemPartGate = new ItemPartGate(Configurator.mp_gateID.getInt());
	}

	@Override
	public void preinit() {}

	@Override
	public void postinit() {}

	@Override
	public TMultiPart createPart(String name, boolean client) {
		if (name == "projred-gate") {
			return new TileGate(EnumGate.AND);
		}
		return null;
	}
	
	
	
	@Override
	public void initRenderings() {}
	@Override
	public void registerEventsAndHandlers() {}
	@Override
	public void initOreDictionaryDefinitions() {}
}
