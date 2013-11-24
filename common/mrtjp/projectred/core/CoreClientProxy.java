package mrtjp.projectred.core;

import net.minecraftforge.common.MinecraftForge;
import codechicken.lib.packet.PacketCustom;
import cpw.mods.fml.client.registry.RenderingRegistry;

public class CoreClientProxy extends CoreProxy {

	@Override
	public void init() {
		super.init();
		MinecraftForge.EVENT_BUS.register(new Messenger());
		PacketCustom.assignHandler(CoreCPH.channel, new CoreCPH());
		
		BasicRenderUtils.coreRenderHandlerID = RenderingRegistry.getNextAvailableRenderId();
		RenderingRegistry.registerBlockHandler(BasicRenderUtils.MultiRenderHandler.instance);
	}
}
