package mrtjp.projectred.integration;

import static mrtjp.projectred.ProjectRed.itemPartGate;
import static mrtjp.projectred.ProjectRed.itemScrewdriver;

import org.apache.commons.lang3.ArrayUtils;

import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.network.NetServerHandler;

import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProxy;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.packet.PacketCustom.IServerPacketHandler;
import codechicken.multipart.MultiPartRegistry;
import codechicken.multipart.MultiPartRegistry.IPartFactory;
import codechicken.multipart.TMultiPart;
import cpw.mods.fml.common.registry.LanguageRegistry;

public class IntegrationProxy implements IProxy, IPartFactory {

	@Override
	public void preinit() {
	}
	
	@Override
	public void init() {
		String[] gates = new String[EnumGate.VALUES.length];
		for (EnumGate g : EnumGate.VALUES) {
			gates[g.ordinal()] = g.name;
		}
		MultiPartRegistry.registerParts(this, gates);

		itemPartGate = new ItemPartGate(Configurator.part_gate.getInt());
		itemScrewdriver = new ItemScrewdriver(Configurator.item_screwdriverID.getInt());
		
		IntegrationRecipes.initIntegrationRecipes();
		EnumGate.initOreDictDefinitions();
		
		PacketCustom.assignHandler(Configurator.integrationPacketChannel, 0, 32, new IntegrationSPH());
	}

	@Override
	public void postinit() {
	}

	@Override
	public TMultiPart createPart(String name, boolean client) {
		EnumGate g = EnumGate.getByName(name);
		if (g != null) {
			return new GatePart(g);
		}
		return null;
	}

	@Override
	public void initRenderings() {
	}

	@Override
	public void registerEventsAndHandlers() {
	}

	@Override
	public void initOreDictionaryDefinitions() {
	}
}
