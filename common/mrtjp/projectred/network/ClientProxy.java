package mrtjp.projectred.network;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.core.ProjectRedTickHandler;
import mrtjp.projectred.multipart.MultipartHighlightHandler;
import mrtjp.projectred.multipart.microblocks.MicroblockItemRenderer;
import mrtjp.projectred.multipart.microblocks.MicroblockPlacementHighlightHandler;
import mrtjp.projectred.multipart.microblocks.MultiblockRenderer;
import mrtjp.projectred.multipart.wiring.gates.GateDynamicRenderer;
import mrtjp.projectred.multipart.wiring.gates.GateStaticRenderer;
import mrtjp.projectred.multipart.wiring.gates.TileGate;
import mrtjp.projectred.multipart.wiring.wires.WireRenderer;
import mrtjp.projectred.renderstuffs.LampRenderer;
import mrtjp.projectred.renderstuffs.RenderIDs;
import mrtjp.projectred.tiles.TileLamp;
import net.minecraftforge.client.MinecraftForgeClient;
import net.minecraftforge.common.MinecraftForge;
import cpw.mods.fml.client.registry.ClientRegistry;
import cpw.mods.fml.client.registry.RenderingRegistry;
import cpw.mods.fml.common.registry.TickRegistry;
import cpw.mods.fml.relauncher.Side;

public class ClientProxy extends CommonProxy implements IProxy {

	@Override
	public void initRenderings() {

		// Lights
		RenderIDs.renderIdLighting = RenderingRegistry.getNextAvailableRenderId();
		ClientRegistry.bindTileEntitySpecialRenderer(TileLamp.class, LampRenderer.instance);

		// Redwire
		RenderIDs.renderIdRedwire = RenderingRegistry.getNextAvailableRenderId();
		RenderingRegistry.registerBlockHandler(WireRenderer.instance);

		// Gates
		RenderIDs.renderIdGate = RenderingRegistry.getNextAvailableRenderId();
		ClientRegistry.bindTileEntitySpecialRenderer(TileGate.class, GateDynamicRenderer.instance);
		RenderingRegistry.registerBlockHandler(GateStaticRenderer.instance);

		// Microblocks
		RenderIDs.renderIdMicroblock = RenderingRegistry.getNextAvailableRenderId();
		RenderingRegistry.registerBlockHandler(MultiblockRenderer.instance);
		MinecraftForgeClient.registerItemRenderer(ProjectRed.blockMicrocontainer.blockID, MicroblockItemRenderer.instance);

	}

	@Override
	public void registerEventsAndHandlers() {
		MinecraftForge.EVENT_BUS.register(new MicroblockPlacementHighlightHandler());
		MinecraftForge.EVENT_BUS.register(new MultipartHighlightHandler());
		TickRegistry.registerTickHandler(ProjectRedTickHandler.instance, Side.CLIENT);
	}

}