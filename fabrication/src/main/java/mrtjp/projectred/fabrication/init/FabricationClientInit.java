package mrtjp.projectred.fabrication.init;

import codechicken.multipart.api.MultipartClientRegistry;
import mrtjp.projectred.fabrication.client.RenderFabricatedGate;
import mrtjp.projectred.fabrication.gui.ICRenderTypes;
import mrtjp.projectred.fabrication.gui.screen.inventory.LithographyTableScreen;
import mrtjp.projectred.fabrication.gui.screen.inventory.PackagingTableScreen;
import mrtjp.projectred.fabrication.gui.screen.inventory.PlottingTableScreen;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.client.GateModelRenderer;
import mrtjp.projectred.integration.client.GatePartRenderer;
import net.minecraft.client.gui.screens.MenuScreens;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.fml.event.lifecycle.FMLClientSetupEvent;
import net.neoforged.neoforge.client.event.RegisterMenuScreensEvent;

import static mrtjp.projectred.fabrication.init.FabricationMenus.*;

@SuppressWarnings("DataFlowIssue")
public class FabricationClientInit {

    public static void init(IEventBus modEventBus) {
        modEventBus.addListener(FabricationClientInit::clientSetup);

        // Register sprites
        modEventBus.addListener(ICRenderTypes::onTextureStitchEvent);

        // Register menu screens
        modEventBus.addListener(FabricationClientInit::onRegisterMenuScreensEvent);
    }

    private static void clientSetup(final FMLClientSetupEvent event) {

        // Register part renderers
        MultipartClientRegistry.register(FabricationParts.FABRICATED_GATE_PART.get(), GatePartRenderer.INSTANCE);

        // Register gate renderer
        GateModelRenderer.registerRenderer(GateType.FABRICATED_GATE, RenderFabricatedGate::new);
    }

    private static void onRegisterMenuScreensEvent(RegisterMenuScreensEvent event) {
        // Register screens
        event.register(PLOTTING_TABLE_MENU.get(), PlottingTableScreen::new);
        event.register(LITHOGRAPHY_TABLE_MENU.get(), LithographyTableScreen::new);
        event.register(PACKAGING_TABLE_MENU.get(), PackagingTableScreen::new);
    }
}
