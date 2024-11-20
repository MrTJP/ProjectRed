package mrtjp.projectred.integration.init;

import codechicken.lib.util.ResourceUtils;
import codechicken.multipart.api.MultipartClientRegistry;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.client.GateModelRenderer;
import mrtjp.projectred.integration.client.GatePartRenderer;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;

public class IntegrationClientInit {

    public static void init() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(IntegrationClientInit::clientSetup);

        // Register sprites
        modEventBus.addListener(GateModelRenderer::onTextureStitchEvent);
    }

    private static void clientSetup(final FMLClientSetupEvent event) {

        for (GateType type : GateType.values()) {
            if (type.isExternalGate()) continue;

            // Register part part renderers
            MultipartClientRegistry.register(type.getPartType(), GatePartRenderer.INSTANCE);
        }

        ResourceUtils.registerReloadListener(GateModelRenderer::onResourceManagerReload);
    }
}
