package mrtjp.projectred.illumination.init;

import codechicken.microblock.api.BlockMicroMaterial;
import codechicken.microblock.util.MicroMaterialRegistry;
import mrtjp.projectred.illumination.BlockLightType;
import mrtjp.projectred.illumination.part.IllumarLampMicroMaterial;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.registries.RegisterEvent;

public class IlluminationMicroMaterials {

    public static void register() {
        // Cannot register deferred since MicroMaterialRegistry.MICRO_MATERIALS registry is null at this point.
        // So we register during registry event below
    }

    @SubscribeEvent
    public void onRegisterMicroMaterials(RegisterEvent event) {
        event.register(MicroMaterialRegistry.MICRO_MATERIALS.getRegistryKey(), r -> {
            for (int color = 0; color < 16; color++) {
                int colorFinal = color;
                IllumarLampMicroMaterial material = new IllumarLampMicroMaterial(() -> BlockLightType.ILLUMAR_LAMP.getBlock(colorFinal, true));
                r.register(BlockMicroMaterial.makeMaterialKey(material.state), material);
            }
        });
    }
}
