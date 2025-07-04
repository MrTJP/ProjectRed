package mrtjp.projectred.exploration.init;

import mrtjp.projectred.exploration.gui.screen.inventory.BackpackScreen;
import net.minecraft.client.gui.screens.MenuScreens;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.fml.event.lifecycle.FMLClientSetupEvent;
import net.neoforged.neoforge.client.event.RegisterMenuScreensEvent;

import static mrtjp.projectred.exploration.init.ExplorationMenus.BACKPACK_MENU;

public class ExplorationClientInit {

    public static void init(IEventBus modEventBus) {
        modEventBus.addListener(ExplorationClientInit::clientSetup);
        modEventBus.addListener(ExplorationClientInit::onRegisterMenuScreensEvent);
    }

    private static void clientSetup(final FMLClientSetupEvent event) {
    }

    private static void onRegisterMenuScreensEvent(RegisterMenuScreensEvent event) {
        // Register screens
        event.register(BACKPACK_MENU.get(), BackpackScreen::new);
    }
}
