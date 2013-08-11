package mrtjp.projectred.transmission;

import static mrtjp.projectred.ProjectRed.itemPartJacketedWire;
import static mrtjp.projectred.ProjectRed.itemPartWire;
import net.minecraftforge.client.MinecraftForgeClient;
import cpw.mods.fml.common.registry.LanguageRegistry;

public class TransmissionClientProxy extends TransmissionProxy {

    @Override
    public void init() {
        for (EnumWire w : EnumWire.VALID_WIRE) {
            LanguageRegistry.addName(w.getItemStack(), w.name);
            if(w.hasJacketedForm())
                LanguageRegistry.addName(w.getJacketedItemStack(), "Jacketed " + w.name);
        }
        MinecraftForgeClient.registerItemRenderer(itemPartWire.itemID, WireItemRenderer.instance);
        //MinecraftForgeClient.registerItemRenderer(itemPartJacketedWire.itemID, JacketedWireItemRenderer.instance);
    }
}
