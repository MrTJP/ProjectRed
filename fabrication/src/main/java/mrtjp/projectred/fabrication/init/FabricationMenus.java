package mrtjp.projectred.fabrication.init;

import codechicken.lib.inventory.container.ICCLContainerType;
import mrtjp.projectred.fabrication.inventory.container.LithographyTableMenu;
import mrtjp.projectred.fabrication.inventory.container.PackagingTableMenu;
import mrtjp.projectred.fabrication.inventory.container.PlottingTableMenu;
import net.minecraft.world.inventory.MenuType;
import net.minecraftforge.registries.RegistryObject;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.MENU_TYPES;
import static mrtjp.projectred.fabrication.init.FabricationBlocks.*;

@SuppressWarnings("NotNullFieldNotInitialized")
public class FabricationMenus {

    public static RegistryObject<MenuType<PlottingTableMenu>> PLOTTING_TABLE_MENU;
    public static RegistryObject<MenuType<LithographyTableMenu>> LITHOGRAPHY_TABLE_MENU;
    public static RegistryObject<MenuType<PackagingTableMenu>> PACKAGING_TABLE_MENU;

    public static void register() {

        PLOTTING_TABLE_MENU = MENU_TYPES.register(ID_PLOTTING_TABLE, () -> ICCLContainerType.create(PlottingTableMenu.FACTORY));
        LITHOGRAPHY_TABLE_MENU = MENU_TYPES.register(ID_LITHOGRAPHY_TABLE, () -> ICCLContainerType.create(LithographyTableMenu.FACTORY));
        PACKAGING_TABLE_MENU = MENU_TYPES.register(ID_PACKAGING_TABLE, () -> ICCLContainerType.create(PackagingTableMenu.FACTORY));
    }
}
