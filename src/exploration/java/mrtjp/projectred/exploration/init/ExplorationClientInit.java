package mrtjp.projectred.exploration.init;

import mrtjp.projectred.exploration.gui.screen.inventory.BackpackScreen;
import net.minecraft.client.gui.ScreenManager;

import static mrtjp.projectred.exploration.init.ExplorationReferences.BACKPACK_CONTAINER;

public class ExplorationClientInit {

    public static void init() {

        ScreenManager.register(BACKPACK_CONTAINER, BackpackScreen::new);
    }
}
