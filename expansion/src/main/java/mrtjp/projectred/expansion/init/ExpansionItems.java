package mrtjp.projectred.expansion.init;

import mrtjp.projectred.expansion.item.BatteryItem;
import mrtjp.projectred.expansion.item.ElectricScrewdriverItem;
import mrtjp.projectred.expansion.item.EmptyBatteryItem;
import mrtjp.projectred.expansion.item.RecipePlanItem;

import static mrtjp.projectred.ProjectRedExpansion.ITEMS;

public class ExpansionItems {

    public static final String ID_RECIPE_PLAN = "recipe_plan";
    public static final String ID_BATTERY = "battery";
    public static final String ID_EMPTY_BATTERY = "empty_battery";
    public static final String ID_ELECTRIC_SCREWDRIVER = "electric_screwdriver";

    public static void register() {

        ITEMS.register(ID_RECIPE_PLAN, RecipePlanItem::new);
        ITEMS.register(ID_BATTERY, BatteryItem::new);
        ITEMS.register(ID_EMPTY_BATTERY, EmptyBatteryItem::new);
        ITEMS.register(ID_ELECTRIC_SCREWDRIVER, ElectricScrewdriverItem::new);
    }
}
