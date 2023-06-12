package mrtjp.projectred.expansion.data;

import net.minecraft.data.DataGenerator;
import net.minecraftforge.common.data.LanguageProvider;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;
import static mrtjp.projectred.expansion.init.ExpansionReferences.*;
import static mrtjp.projectred.expansion.init.ExpansionUnlocal.UL_PLAN_RESULT;
import static mrtjp.projectred.expansion.init.ExpansionUnlocal.UL_STORED_POWER_TOOLTIP;

public class ExpansionLanguageProvider extends LanguageProvider {

    public ExpansionLanguageProvider(DataGenerator gen) {
        super(gen, MOD_ID, "en_us");
    }

    @Override
    public String getName() {
        return "ProjectRed-Expansion Languages: en_us";
    }

    @Override
    protected void addTranslations() {

        // Creative tab
        add("itemGroup." + MOD_ID, "Project Red: Expansion");

        add(PROJECT_BENCH_BLOCK, "Project Bench");
        add(BATTERY_BOX_BLOCK, "Battery Box");
        add(AUTO_CRAFTER_BLOCK, "Auto Crafter");
        add(CHARGING_BENCH_BLOCK, "Charging Bench");
        add(FIRE_STARTER_BLOCK, "Fire Starter");
        add(FRAME_BLOCK, "Frame");
        add(FRAME_MOTOR_BLOCK, "Frame Motor");
        add(FRAME_ACTUATOR_BLOCK, "Frame Actuator");

        add(RECIPE_PLAN_ITEM, "Recipe Plan");
        add(EMPTY_BATTERY_ITEM, "Empty Battery");
        add(BATTERY_ITEM, "Battery");
        add(ELECTRIC_SCREWDRIVER_ITEM, "Electric Screwdriver");

        // Strings
        add(UL_STORED_POWER_TOOLTIP, "Stored power");
        add(UL_PLAN_RESULT, "Result");
    }
}
