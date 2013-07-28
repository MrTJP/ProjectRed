package mrtjp.projectred.network;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.core.IProjectRedModule;
import mrtjp.projectred.core.Messenger;
import mrtjp.projectred.core.ProjectRedTickHandler;
import mrtjp.projectred.integration.ModuleIntegration;
import mrtjp.projectred.multipart.MultipartHighlightHandler;
import mrtjp.projectred.multipart.microblocks.MicroblockItemRenderer;
import mrtjp.projectred.multipart.microblocks.MicroblockPlacementHighlightHandler;
import mrtjp.projectred.multipart.microblocks.MultiblockRenderer;
import mrtjp.projectred.multipart.wiring.wires.WireRenderer;
import mrtjp.projectred.renderstuffs.LampRenderer;
import mrtjp.projectred.renderstuffs.LanternRenderer;
import mrtjp.projectred.renderstuffs.RenderIDs;
import mrtjp.projectred.tiles.TileLamp;
import mrtjp.projectred.tiles.TileLantern;
import net.minecraftforge.client.MinecraftForgeClient;
import net.minecraftforge.common.MinecraftForge;
import cpw.mods.fml.client.registry.ClientRegistry;
import cpw.mods.fml.client.registry.RenderingRegistry;
import cpw.mods.fml.common.registry.TickRegistry;
import cpw.mods.fml.relauncher.Side;

public class ClientProxy extends CommonProxy implements IProxy {

	public static int renderPass;
	
	@Override
	public void init() {
		IProjectRedModule m = new ModuleIntegration();
		m.getClientProxy().init();
	}

	@Override
	public void preinit() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void postinit() {
		// TODO Auto-generated method stub
		
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
		
		// Redwire
		RenderIDs.renderIdRedwire = RenderingRegistry.getNextAvailableRenderId();
		RenderingRegistry.registerBlockHandler(WireRenderer.instance);

		// Gates
		/**
		RenderIDs.renderIdGate = RenderingRegistry.getNextAvailableRenderId();
		ClientRegistry.bindTileEntitySpecialRenderer(TileGate.class, GateDynamicRenderer.instance);
		RenderingRegistry.registerBlockHandler(GateStaticRenderer.instance);
		MinecraftForgeClient.registerItemRenderer(ProjectRed.blockGate.blockID, GateStaticRenderer.instance);
        **/
		// Microblocks
		RenderIDs.renderIdMicroblock = RenderingRegistry.getNextAvailableRenderId();
		RenderingRegistry.registerBlockHandler(MultiblockRenderer.instance);
		MinecraftForgeClient.registerItemRenderer(ProjectRed.blockMicrocontainer.blockID, MicroblockItemRenderer.instance);

	}

	@Override
	public void registerEventsAndHandlers() {
		MinecraftForge.EVENT_BUS.register(new MicroblockPlacementHighlightHandler());
		MinecraftForge.EVENT_BUS.register(new MultipartHighlightHandler());
		MinecraftForge.EVENT_BUS.register(new Messenger());
		TickRegistry.registerTickHandler(ProjectRedTickHandler.instance, Side.CLIENT);
	}

}