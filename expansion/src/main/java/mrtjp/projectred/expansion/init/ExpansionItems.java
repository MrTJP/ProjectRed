package mrtjp.projectred.expansion.init;

import mrtjp.projectred.expansion.item.BatteryItem;
import mrtjp.projectred.expansion.item.ElectricScrewdriverItem;
import mrtjp.projectred.expansion.item.EmptyBatteryItem;
import mrtjp.projectred.expansion.item.RecipePlanItem;
import net.minecraft.world.item.Item;

import java.util.function.Supplier;

import static mrtjp.projectred.expansion.ProjectRedExpansion.ITEMS;

@SuppressWarnings("NotNullFieldNotInitialized")
public class ExpansionItems {

    public static final String ID_RECIPE_PLAN = "recipe_plan";
    public static final String ID_BATTERY = "battery";
    public static final String ID_EMPTY_BATTERY = "empty_battery";
    public static final String ID_ELECTRIC_SCREWDRIVER = "electric_screwdriver";

    public static Supplier<Item> RECIPE_PLAN_ITEM;
    public static Supplier<Item> BATTERY_ITEM;
    public static Supplier<Item> EMPTY_BATTERY_ITEM;
    public static Supplier<Item> ELECTRIC_SCREWDRIVER_ITEM;


    public static void register() {

        RECIPE_PLAN_ITEM = ITEMS.register(ID_RECIPE_PLAN, RecipePlanItem::new);
        BATTERY_ITEM = ITEMS.register(ID_BATTERY, BatteryItem::new);
        EMPTY_BATTERY_ITEM = ITEMS.register(ID_EMPTY_BATTERY, EmptyBatteryItem::new);
        ELECTRIC_SCREWDRIVER_ITEM = ITEMS.register(ID_ELECTRIC_SCREWDRIVER, ElectricScrewdriverItem::new);
    }
}
