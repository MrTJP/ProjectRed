package mrtjp.projectred.transmission;

import static mrtjp.projectred.ProjectRed.*;
import mrtjp.projectred.core.IProxy;
import static mrtjp.projectred.ProjectRedTransmission.itemPartWire;
import mrtjp.projectred.ProjectRedCore;
import mrtjp.projectred.ProjectRed;
import net.minecraft.item.ItemStack;
import net.minecraftforge.client.MinecraftForgeClient;
import cpw.mods.fml.common.registry.LanguageRegistry;

public class TransmissionClientProxy extends TransmissionProxy {

    @Override
    public void init() {
        super.init();
        for (EnumWire w : EnumWire.VALID_WIRE) {
            LanguageRegistry.addName(w.getItemStack(), w.name);
            if (w.hasJacketedForm())
                LanguageRegistry.addName(w.getJacketedItemStack(), "Jacketed " + w.name);
        }
        LanguageRegistry.addName(new ItemStack(ProjectRedCore.itemWireDebugger, 1, 0), "Wire debugger");
        MinecraftForgeClient.registerItemRenderer(itemPartWire.itemID, WireItemRenderer.instance);
        MinecraftForgeClient.registerItemRenderer(itemPartJacketedWire.itemID, JacketedWireItemRenderer.instance);
    }
}
