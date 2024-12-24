package mrtjp.projectred.exploration.init;

import mrtjp.projectred.exploration.gui.screen.inventory.BackpackScreen;
import net.minecraft.client.gui.screens.MenuScreens;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.fml.event.lifecycle.FMLClientSetupEvent;

import static mrtjp.projectred.exploration.init.ExplorationMenus.BACKPACK_MENU;

public class ExplorationClientInit {

    public static void init(IEventBus modEventBus) {
        modEventBus.addListener(ExplorationClientInit::clientSetup);
    }

    private static void clientSetup(final FMLClientSetupEvent event) {

        // Register screens
        MenuScreens.register(BACKPACK_MENU.get(), BackpackScreen::new);
    }
}
