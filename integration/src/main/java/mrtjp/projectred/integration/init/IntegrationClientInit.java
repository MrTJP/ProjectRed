package mrtjp.projectred.integration.init;

import codechicken.lib.model.ModelRegistryHelper;
import codechicken.lib.texture.SpriteRegistryHelper;
import codechicken.lib.util.ResourceUtils;
import codechicken.multipart.api.MultipartClientRegistry;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.client.GateModelRenderer;
import mrtjp.projectred.integration.client.GatePartItemRenderer;
import mrtjp.projectred.integration.client.GatePartRenderer;
import net.minecraft.client.resources.model.ModelResourceLocation;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;

import java.util.Objects;

public class IntegrationClientInit {

    private static final ModelRegistryHelper MODEL_HELPER = new ModelRegistryHelper();

    public static void init() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(IntegrationClientInit::clientSetup);

        // Register sprites
        SpriteRegistryHelper spriteHelper = new SpriteRegistryHelper(modEventBus);
        spriteHelper.addIIconRegister(GateModelRenderer::registerIcons);
    }

    private static void clientSetup(final FMLClientSetupEvent event) {

        for (GateType type : GateType.values()) {
            if (type.isExternalGate()) continue;

            // Register part block renderers
            MultipartClientRegistry.register(type.getPartType(), GatePartRenderer.INSTANCE);

            // Register part item renderer
            MODEL_HELPER.register(new ModelResourceLocation(Objects.requireNonNull(type.getItemRegistryObject().getId()), "inventory"), GatePartItemRenderer.INSTANCE);
        }

        ResourceUtils.registerReloadListener(GateModelRenderer::onResourceManagerReload);
    }
}
