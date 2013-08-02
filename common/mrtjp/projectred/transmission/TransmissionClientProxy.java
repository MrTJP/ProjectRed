package mrtjp.projectred.transmission;

import cpw.mods.fml.common.registry.LanguageRegistry;
import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.integration.GateStaticRenderer;
import net.minecraftforge.client.MinecraftForgeClient;

public class TransmissionClientProxy extends TransmissionProxy {

	@Override
	public void init() {
		for (EnumWire w : EnumWire.VALID_WIRE) {
			LanguageRegistry.addName(w.getItemStack(), w.name);
		}
		MinecraftForgeClient.registerItemRenderer(ProjectRed.itemPartWire.itemID, WireItemRenderer.instance);

	}
}
