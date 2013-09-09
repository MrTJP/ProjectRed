package mrtjp.projectred.transmission;

import static mrtjp.projectred.ProjectRedTransmission.itemPartFramedWire;
import static mrtjp.projectred.ProjectRedTransmission.itemPartWire;
import net.minecraftforge.client.MinecraftForgeClient;
import codechicken.microblock.MicroMaterialRegistry;

public class TransmissionClientProxy extends TransmissionProxy {

    @Override
    public void init() {
        super.init();
        MinecraftForgeClient.registerItemRenderer(itemPartWire.itemID, WireItemRenderer.instance);
        MinecraftForgeClient.registerItemRenderer(itemPartFramedWire.itemID, FramedWireItemRenderer.instance);
        MicroMaterialRegistry.registerHighlightRenderer(new JacketedHighlightRenderer());
    }
}
