package mrtjp.projectred.illumination.init;

import codechicken.microblock.api.MicroMaterial;
import mrtjp.projectred.illumination.BlockLightType;
import mrtjp.projectred.illumination.part.IllumarLampMicroMaterial;
import net.minecraftforge.event.RegistryEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;

public class IlluminationMicroMaterials {

    public static void register() {
        // Register mixin
        // Nothing to do. IllumarLampMicroblock is registered via annotation
    }

    @SubscribeEvent
    @SuppressWarnings({"unchecked","rawtypes"})
    public void onRegisterMicroMaterials(RegistryEvent.Register event) {
        if (event.getGenericType() == MicroMaterial.class) {
            // Illumar lamp microblocks
            for (int color = 0; color < 16; color++) {
                int colorFinal = color;
                event.getRegistry().register(new IllumarLampMicroMaterial(() -> BlockLightType.ILLUMAR_LAMP.getBlock(colorFinal, true)));
            }
        }
    }
}
