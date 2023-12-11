package mrtjp.projectred.expansion.init;

import codechicken.lib.inventory.container.ICCLContainerType;
import mrtjp.projectred.expansion.inventory.container.AutoCrafterContainer;
import mrtjp.projectred.expansion.inventory.container.BatteryBoxContainer;
import mrtjp.projectred.expansion.inventory.container.ChargingBenchContainer;
import mrtjp.projectred.expansion.inventory.container.ProjectBenchContainer;
import net.minecraft.world.inventory.MenuType;
import net.minecraftforge.registries.RegistryObject;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MENU_TYPES;
import static mrtjp.projectred.expansion.init.ExpansionBlocks.*;

@SuppressWarnings("NotNullFieldNotInitialized")
public class ExpansionMenus {

    public static RegistryObject<MenuType<ProjectBenchContainer>> PROJECT_BENCH_CONTAINER;
    public static RegistryObject<MenuType<BatteryBoxContainer>> BATTERY_BOX_CONTAINER;
    public static RegistryObject<MenuType<AutoCrafterContainer>> AUTO_CRAFTER_CONTAINER;
    public static RegistryObject<MenuType<ChargingBenchContainer>> CHARGING_BENCH_CONTAINER;

    public static void register() {

        PROJECT_BENCH_CONTAINER = MENU_TYPES.register(ID_PROJECT_BENCH, () -> ICCLContainerType.create(ProjectBenchContainer.FACTORY));
        BATTERY_BOX_CONTAINER = MENU_TYPES.register(ID_BATTERY_BOX, () -> ICCLContainerType.create(BatteryBoxContainer.FACTORY));
        AUTO_CRAFTER_CONTAINER = MENU_TYPES.register(ID_AUTO_CRAFTER, () -> ICCLContainerType.create(AutoCrafterContainer.FACTORY));
        CHARGING_BENCH_CONTAINER = MENU_TYPES.register(ID_CHARGING_BENCH, () -> ICCLContainerType.create(ChargingBenchContainer.FACTORY));
    }
}
