package mrtjp.projectred.network;

import static mrtjp.projectred.ProjectRed.initializedModules;
import static mrtjp.projectred.ProjectRed.registeredModules;
import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.core.IProjectRedModule;
import mrtjp.projectred.core.Messenger;
import mrtjp.projectred.core.ProjectRedTickHandler;
import mrtjp.projectred.integration.ModuleIntegration;
import mrtjp.projectred.multipart.MultipartHighlightHandler;
import mrtjp.projectred.multipart.microblocks.MicroblockItemRenderer;
import mrtjp.projectred.multipart.microblocks.MicroblockPlacementHighlightHandler;
import mrtjp.projectred.multipart.microblocks.MultiblockRenderer;
import mrtjp.projectred.renderstuffs.LampRenderer;
import mrtjp.projectred.renderstuffs.LanternRenderer;
import mrtjp.projectred.renderstuffs.RenderIDs;
import mrtjp.projectred.tiles.TileLamp;
import mrtjp.projectred.tiles.TileLantern;
import mrtjp.projectred.transmission.WireItemRenderer;
import net.minecraftforge.client.MinecraftForgeClient;
import net.minecraftforge.common.MinecraftForge;
import cpw.mods.fml.client.registry.ClientRegistry;
import cpw.mods.fml.client.registry.RenderingRegistry;
import cpw.mods.fml.common.registry.TickRegistry;
import cpw.mods.fml.relauncher.Side;

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

		// Microblocks
		RenderIDs.renderIdMicroblock = RenderingRegistry.getNextAvailableRenderId();
		RenderingRegistry.registerBlockHandler(MultiblockRenderer.instance);
		MinecraftForgeClient.registerItemRenderer(ProjectRed.blockMicrocontainer.blockID, MicroblockItemRenderer.instance);

	}

	@Override
	public void registerEventsAndHandlers() {
		MinecraftForge.EVENT_BUS.register(new MicroblockPlacementHighlightHandler());
		MinecraftForge.EVENT_BUS.register(new MultipartHighlightHandler());
	}

}