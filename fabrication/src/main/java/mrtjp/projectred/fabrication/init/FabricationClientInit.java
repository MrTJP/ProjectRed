package mrtjp.projectred.fabrication.init;

import codechicken.multipart.api.MultipartClientRegistry;
import mrtjp.projectred.fabrication.gui.ICRenderTypes;
import mrtjp.projectred.fabrication.gui.screen.inventory.LithographyTableScreen;
import mrtjp.projectred.fabrication.gui.screen.inventory.PackagingTableScreen;
import mrtjp.projectred.fabrication.gui.screen.inventory.PlottingTableScreen;
import mrtjp.projectred.integration.client.GatePartRenderer;
import net.minecraft.client.gui.screens.MenuScreens;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.fml.event.lifecycle.FMLClientSetupEvent;

import static mrtjp.projectred.fabrication.init.FabricationMenus.*;

@SuppressWarnings("DataFlowIssue")
public class FabricationClientInit {

    public static void init(IEventBus modEventBus) {
        modEventBus.addListener(FabricationClientInit::clientSetup);

        // Register sprites
        modEventBus.addListener(ICRenderTypes::onTextureStitchEvent);
    }

    private static void clientSetup(final FMLClientSetupEvent event) {

        // Register part renderers
        MultipartClientRegistry.register(FabricationParts.FABRICATED_GATE_PART.get(), GatePartRenderer.INSTANCE);

        // Register screens
        MenuScreens.register(PLOTTING_TABLE_MENU.get(), PlottingTableScreen::new);
        MenuScreens.register(LITHOGRAPHY_TABLE_MENU.get(), LithographyTableScreen::new);
        MenuScreens.register(PACKAGING_TABLE_MENU.get(), PackagingTableScreen::new);
    }
}
