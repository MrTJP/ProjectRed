package mrtjp.projectred.illumination;

import cpw.mods.fml.common.registry.LanguageRegistry;
import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.integration.GateStaticRenderer;
import net.minecraftforge.client.MinecraftForgeClient;
import net.minecraftforge.common.MinecraftForge;

public class IlluminationClientProxy extends IlluminationProxy {

	@Override
	public void init() {
		MinecraftForgeClient.registerItemRenderer(ProjectRed.itemPartLantern.itemID, LanternRenderer.instance);
		MinecraftForgeClient.registerItemRenderer(ProjectRed.itemPartInvLantern.itemID, LanternRenderer.instance);
		for (EnumLantern e : EnumLantern.values()) {
			LanguageRegistry.addName(e.getItemStack(), e.fullName);
			LanguageRegistry.addName(e.getInvertedItemStack(), "Inverted " + e.fullName);			
		}
		
		MinecraftForge.EVENT_BUS.register(LastEventBasedHaloRenderer.instance);
	}
}
