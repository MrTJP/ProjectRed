package mrtjp.projectred.core;

import static mrtjp.projectred.ProjectRed.initializedModules;
import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.renderstuffs.LampRenderer;
import mrtjp.projectred.renderstuffs.LanternRenderer;
import mrtjp.projectred.renderstuffs.RenderIDs;
import mrtjp.projectred.tiles.TileLamp;
import mrtjp.projectred.tiles.TileLantern;
import net.minecraftforge.client.MinecraftForgeClient;
import cpw.mods.fml.client.registry.ClientRegistry;
import cpw.mods.fml.client.registry.RenderingRegistry;

public class ClientProxy extends CommonProxy {

	public static int renderPass;

	@Override
	public void preinit() {
		super.preinit();
		for (IProjectRedModule m : initializedModules) {
			m.getClientProxy().preinit();
		}
	}

	@Override
	public void init() {
		super.init();
		for (IProjectRedModule m : initializedModules) {
			m.getClientProxy().init();
		}
	}

	@Override
	public void postinit() {
		super.postinit();
		for (IProjectRedModule m : initializedModules) {
			m.getCommonProxy().postinit();
		}
	}

	@Override
	public void initRenderings() {

		// Lamps
		RenderIDs.renderIdLamp = RenderingRegistry.getNextAvailableRenderId();
		ClientRegistry.bindTileEntitySpecialRenderer(TileLamp.class, LampRenderer.instance);

		// Lanterns
		RenderIDs.renderIDLantern = RenderingRegistry.getNextAvailableRenderId();
		ClientRegistry.bindTileEntitySpecialRenderer(TileLantern.class, LanternRenderer.instance);
		MinecraftForgeClient.registerItemRenderer(ProjectRed.blockLantern.blockID, LanternRenderer.instance);

	}

	@Override
	public void registerEventsAndHandlers() {
	}

}