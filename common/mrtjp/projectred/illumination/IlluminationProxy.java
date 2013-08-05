package mrtjp.projectred.illumination;

import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProxy;
import static mrtjp.projectred.ProjectRed.*;
import codechicken.multipart.MultiPartRegistry.IPartFactory;
import codechicken.multipart.MultiPartRegistry;
import codechicken.multipart.TMultiPart;

public class IlluminationProxy implements IProxy, IPartFactory {

	@Override
	public void preinit() {

	}

	@Override
	public void init() {
		MultiPartRegistry.registerParts(this, new String[] {"Lantern", "inv.Lantern"});
		itemPartLantern = new ItemPartLantern(Configurator.part_lantern.getInt(), false);
		itemPartInvLantern = new ItemPartLantern(Configurator.part_invlantern.getInt(), true);
	
		EnumLantern.initOreDictDefinitions();
	}

	@Override
	public void postinit() {

	}

	@Override
	public TMultiPart createPart(String name, boolean arg1) {
		boolean inverted = false;
		if (name.startsWith("inv.")) {
			name = name.substring(4);
			inverted = true;
		}
		return new LanternPart(EnumLantern.WHITE, inverted, 0);
	}
}
