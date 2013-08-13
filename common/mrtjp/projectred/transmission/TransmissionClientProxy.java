package mrtjp.projectred.transmission;

import static mrtjp.projectred.ProjectRed.itemPartJacketedWire;
import static mrtjp.projectred.ProjectRed.itemPartWire;
import mrtjp.projectred.core.IProxy;
import net.minecraftforge.client.MinecraftForgeClient;
import cpw.mods.fml.common.registry.LanguageRegistry;

public class TransmissionClientProxy implements IProxy {

    @Override
    public void preinit() {}

    @Override
    public void init() {
        for (EnumWire w : EnumWire.VALID_WIRE) {
            LanguageRegistry.addName(w.getItemStack(), w.name);
            LanguageRegistry.addName(w.getJacketedItemStack(), "Jacketed " + w.name);
        }
        MinecraftForgeClient.registerItemRenderer(itemPartWire.itemID, WireItemRenderer.instance);
        MinecraftForgeClient.registerItemRenderer(itemPartJacketedWire.itemID, JacketedWireItemRenderer.instance);
    }

    @Override
    public void postinit() {}
}
