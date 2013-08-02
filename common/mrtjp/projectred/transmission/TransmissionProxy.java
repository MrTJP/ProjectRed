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
		String[] wires = new String[EnumWire.VALID_WIRE.length];
		for (EnumWire w : EnumWire.VALID_WIRE) {
			wires[w.meta] = w.name;
		}
		MultiPartRegistry.registerParts(this, wires);
		
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
		boolean isJacketed = false;
		EnumWire w = EnumWire.getTypeByName(id);
		try {
			return (TMultiPart) w.teclass.getConstructors()[0].newInstance(w, isJacketed, 0);
		} catch (Throwable e) {
			return null;
		}
	}

}
