package mrtjp.projectred.exploration.init;

import codechicken.lib.inventory.container.ICCLContainerType;
import mrtjp.projectred.exploration.inventory.container.BackpackContainer;
import net.minecraft.world.inventory.MenuType;
import net.minecraftforge.registries.RegistryObject;

import static mrtjp.projectred.exploration.ProjectRedExploration.MENU_TYPES;

@SuppressWarnings("NotNullFieldNotInitialized")
public class ExplorationMenus {

    public static final String ID_BACKPACK_CONTAINER = "backpack";

    public static RegistryObject<MenuType<BackpackContainer>> BACKPACK_CONTAINER;

    public static void register() {

        BACKPACK_CONTAINER = MENU_TYPES.register(ID_BACKPACK_CONTAINER, () -> ICCLContainerType.create(BackpackContainer.FACTORY));
    }
}
