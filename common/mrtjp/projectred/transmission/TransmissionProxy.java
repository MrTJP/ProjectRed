package mrtjp.projectred.transmission;

import static mrtjp.projectred.ProjectRed.itemPartJacketedWire;
import static mrtjp.projectred.ProjectRed.itemPartWire;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProxy;
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
		String[] jwires = new String[EnumWire.VALID_WIRE.length];
		
		for (EnumWire w : EnumWire.VALID_WIRE) {
			wires[w.meta] = w.name;
			jwires[w.meta] = "j." + w.name;
		}
		MultiPartRegistry.registerParts(this, wires);
		MultiPartRegistry.registerParts(this, jwires);
		
		itemPartWire = new ItemPartWire(Configurator.part_wire.getInt());
		itemPartJacketedWire = new ItemPartJacketedWire(Configurator.part_jwire.getInt());
		
		TransmissionRecipes.initTransmissionRecipes();
		EnumWire.initOreDictDefinitions();
	}

	@Override
	public void postinit() {

	}

	@Override
	public TMultiPart createPart(String id, boolean arg1) {
		boolean isJacketed = false;
		if (id.startsWith("j.")) {
			isJacketed = true;
			id = id.substring(2);
		}
		EnumWire w = EnumWire.getTypeByName(id);
		try {
			return (TMultiPart) w.teclass.getConstructors()[0].newInstance(w, isJacketed, 0);
		} catch (Throwable e) {
			return null;
		}
	}

}
