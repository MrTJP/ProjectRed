package mrtjp.projectred.transmission;

import static mrtjp.projectred.ProjectRed.itemPartWire;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.network.IProxy;
import codechicken.multipart.MultiPartRegistry;
import codechicken.multipart.MultiPartRegistry.IPartFactory;
import codechicken.multipart.TMultiPart;

public class TransmissionProxy implements IProxy, IPartFactory {

	@Override
	public void preinit() {

	}

	@Override
	public void init() {
		MultiPartRegistry.registerParts(this, new String[] {EnumWire.RED_ALLOY.name});
		itemPartWire = new ItemPartWire(Configurator.part_wire.getInt());
	}

	@Override
	public void postinit() {

	}

	@Override
	public void initRenderings() {
		// TODO Auto-generated method stub

	}

	@Override
	public void registerEventsAndHandlers() {
		// TODO Auto-generated method stub

	}

	@Override
	public void initOreDictionaryDefinitions() {
		// TODO Auto-generated method stub

	}

	@Override
	public TMultiPart createPart(String id, boolean arg1) {
		WirePart p = new RedAlloyWirePart();
		p.setWireType(EnumWire.RED_ALLOY);
		return p;
	}

}
