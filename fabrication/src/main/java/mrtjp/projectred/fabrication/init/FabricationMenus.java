package mrtjp.projectred.fabrication.init;

import codechicken.lib.inventory.container.ICCLContainerType;
import mrtjp.projectred.fabrication.inventory.container.LithographyTableContainer;
import mrtjp.projectred.fabrication.inventory.container.PackagingTableContainer;
import mrtjp.projectred.fabrication.inventory.container.PlottingTableContainer;
import net.minecraft.world.inventory.MenuType;
import net.minecraftforge.registries.RegistryObject;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.MENU_TYPES;
import static mrtjp.projectred.fabrication.init.FabricationBlocks.*;

@SuppressWarnings("NotNullFieldNotInitialized")
public class FabricationMenus {

    public static RegistryObject<MenuType<PlottingTableContainer>> PLOTTING_TABLE_CONTAINER;
    public static RegistryObject<MenuType<LithographyTableContainer>> LITHOGRAPHY_TABLE_CONTAINER;
    public static RegistryObject<MenuType<PackagingTableContainer>> PACKAGING_TABLE_CONTAINER;

    public static void register() {

        PLOTTING_TABLE_CONTAINER = MENU_TYPES.register(ID_PLOTTING_TABLE, () -> ICCLContainerType.create(PlottingTableContainer.FACTORY));
        LITHOGRAPHY_TABLE_CONTAINER = MENU_TYPES.register(ID_LITHOGRAPHY_TABLE, () -> ICCLContainerType.create(LithographyTableContainer.FACTORY));
        PACKAGING_TABLE_CONTAINER = MENU_TYPES.register(ID_PACKAGING_TABLE, () -> ICCLContainerType.create(PackagingTableContainer.FACTORY));
    }
}
