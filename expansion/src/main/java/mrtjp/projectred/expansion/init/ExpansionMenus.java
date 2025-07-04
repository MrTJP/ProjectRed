package mrtjp.projectred.expansion.init;

import codechicken.lib.inventory.container.CCLMenuType;
import mrtjp.projectred.expansion.inventory.container.*;
import net.minecraft.world.inventory.MenuType;

import java.util.function.Supplier;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MENU_TYPES;
import static mrtjp.projectred.expansion.init.ExpansionBlocks.*;

@SuppressWarnings("NotNullFieldNotInitialized")
public class ExpansionMenus {

    public static Supplier<MenuType<ProjectBenchMenu>> PROJECT_BENCH_MENU;
    public static Supplier<MenuType<BatteryBoxMenu>> BATTERY_BOX_MENU;
    public static Supplier<MenuType<AutoCrafterMenu>> AUTO_CRAFTER_MENU;
    public static Supplier<MenuType<ChargingBenchMenu>> CHARGING_BENCH_MENU;
    public static Supplier<MenuType<DeployerMenu>> DEPLOYER_MENU;

    public static void register() {

        PROJECT_BENCH_MENU = MENU_TYPES.register(ID_PROJECT_BENCH, () -> CCLMenuType.create(ProjectBenchMenu.FACTORY));
        BATTERY_BOX_MENU = MENU_TYPES.register(ID_BATTERY_BOX, () -> CCLMenuType.create(BatteryBoxMenu.FACTORY));
        AUTO_CRAFTER_MENU = MENU_TYPES.register(ID_AUTO_CRAFTER, () -> CCLMenuType.create(AutoCrafterMenu.FACTORY));
        CHARGING_BENCH_MENU = MENU_TYPES.register(ID_CHARGING_BENCH, () -> CCLMenuType.create(ChargingBenchMenu.FACTORY));
        DEPLOYER_MENU = MENU_TYPES.register(ID_DEPLOYER, () -> CCLMenuType.create(DeployerMenu.FACTORY));
    }
}
