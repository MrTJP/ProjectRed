package mrtjp.projectred.core.init;

import codechicken.lib.inventory.container.ICCLContainerType;
import mrtjp.projectred.core.inventory.container.ElectrotineGeneratorContainer;
import net.minecraft.world.inventory.MenuType;
import net.minecraftforge.registries.RegistryObject;

import static mrtjp.projectred.core.ProjectRedCore.MENU_TYPES;
import static mrtjp.projectred.core.init.CoreBlocks.ID_ELECTROTINE_GENERATOR;

@SuppressWarnings("NotNullFieldNotInitialized")
public class CoreMenus {

    public static RegistryObject<MenuType<ElectrotineGeneratorContainer>> ELECTROTINE_GENERATOR_CONTAINER;

    public static void register() {

        ELECTROTINE_GENERATOR_CONTAINER = MENU_TYPES.register(ID_ELECTROTINE_GENERATOR, () -> ICCLContainerType.create(ElectrotineGeneratorContainer.FACTORY));
    }
}
