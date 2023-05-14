package mrtjp.projectred.core.init;

import mrtjp.projectred.core.client.HaloRenderer;
import mrtjp.projectred.core.gui.screen.inventory.ElectrotineGeneratorScreen;
import net.minecraft.client.gui.ScreenManager;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;

import static mrtjp.projectred.core.init.CoreReferences.ELECTROTINE_GENERATOR_CONTAINER;

public class CoreClientInit {

    public static void init() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(CoreClientInit::clientSetup);
    }

    private static void clientSetup(final FMLClientSetupEvent event) {

        // Register screens
        ScreenManager.register(ELECTROTINE_GENERATOR_CONTAINER, ElectrotineGeneratorScreen::new);

        // Register Halo renderer
        MinecraftForge.EVENT_BUS.addListener(HaloRenderer::onRenderWorldLastEvent);
    }
}
