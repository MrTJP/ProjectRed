package mrtjp.projectred.exploration.init;

import mrtjp.projectred.exploration.gui.screen.inventory.BackpackScreen;
import net.minecraft.client.gui.screens.MenuScreens;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;

import static mrtjp.projectred.exploration.init.ExplorationMenus.BACKPACK_MENU;

public class ExplorationClientInit {

    public static void init() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(ExplorationClientInit::clientSetup);
    }

    private static void clientSetup(final FMLClientSetupEvent event) {

        // Register screens
        MenuScreens.register(BACKPACK_MENU.get(), BackpackScreen::new);
    }
}
