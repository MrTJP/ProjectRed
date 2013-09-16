package mrtjp.projectred.core;

import mrtjp.projectred.core.BlockBasics.EnumBasics;
import net.minecraftforge.common.MinecraftForge;
import codechicken.lib.packet.PacketCustom;
import cpw.mods.fml.client.registry.ClientRegistry;
import cpw.mods.fml.client.registry.RenderingRegistry;

public class CoreClientProxy extends CoreProxy {

	public static int basicRenderID = 0;

	@Override
	public void init() {
		super.init();
		MinecraftForge.EVENT_BUS.register(new Messenger());
		PacketCustom.assignHandler(CoreCPH.channel, new CoreCPH());
		ClientRegistry.bindTileEntitySpecialRenderer(EnumBasics.ALLOYSMELTER.clazz, new AlloySmelterTESR());

		basicRenderID = RenderingRegistry.getNextAvailableRenderId();
		RenderingRegistry.registerBlockHandler(new BasicsISBRH());
	}
}
