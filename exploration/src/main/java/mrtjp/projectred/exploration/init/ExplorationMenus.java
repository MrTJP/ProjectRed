package mrtjp.projectred.exploration.init;

import codechicken.lib.inventory.container.ICCLContainerType;
import mrtjp.projectred.exploration.inventory.container.BackpackMenu;
import net.minecraft.world.inventory.MenuType;

import java.util.function.Supplier;

import static mrtjp.projectred.exploration.ProjectRedExploration.MENU_TYPES;

@SuppressWarnings("NotNullFieldNotInitialized")
public class ExplorationMenus {

    public static final String ID_BACKPACK_CONTAINER = "backpack";

    public static Supplier<MenuType<BackpackMenu>> BACKPACK_MENU;

    public static void register() {

        BACKPACK_MENU = MENU_TYPES.register(ID_BACKPACK_CONTAINER, () -> ICCLContainerType.create(BackpackMenu.FACTORY));
    }
}
